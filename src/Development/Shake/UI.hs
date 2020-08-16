module Development.Shake.UI where

import qualified Brick as B
import qualified Brick.Main as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Util as B
import qualified Graphics.Vty.Attributes.Color as B
import qualified Brick.Widgets.Border as B
import qualified Brick.BChan as B
import qualified Graphics.Vty.Input.Events as V
import qualified Graphics.Vty as V
import qualified Brick.Widgets.ProgressBar as B
import Data.Time
import Data.Time.Format
import Development.Shake.UI.Types
import Development.Shake
import Development.Shake.FilePath
import qualified System.FSNotify as Notify
import qualified Data.List as DL
import UnliftIO.MVar
import System.Directory (makeAbsolute)
import UnliftIO.Async
import Control.Monad
import UnliftIO.Exception (SomeException, try)
import Control.Concurrent (threadDelay)


defaultMain :: UIOpts -> IO ()
defaultMain uiOpts@UIOpts{..} = do
  tz <- getCurrentTimeZone
  let buildVty = V.mkVty V.defaultConfig
      brickApp :: B.App UIState UIEvent UIRef
      brickApp = B.App
                 { B.appDraw = renderBrickApp
                 , B.appChooseCursor = (const $ const Nothing)
                 , B.appHandleEvent = handleBrickEvent
                 , B.appStartEvent = \s -> do
                     B.vScrollToEnd $ B.viewportScroll FileWatcherViewport
                     pure s
                 , B.appAttrMap = (const brickAttrMap)
                 }
      uiFileWatcherBuffer = FixedLenthList { fllLength = 10, fllElements = [] }
      uiShakeBuffer = FixedLenthList { fllLength = 1000, fllElements = [] }
      uiToLocalTime = utcToLocalTime tz
      uiShakeProgress = Nothing
  uiAbsoluteSourceDir <- makeAbsolute optSourceDir
  initialVty <- buildVty
  eventChan <- B.newBChan 100
  withAsync (runFileWatcher uiOpts eventChan) $ \_ -> do
    void $ B.customMain initialVty buildVty (Just eventChan) brickApp UIState{..}


renderBrickApp :: UIState -> [B.Widget UIRef]
renderBrickApp uiState =
  [ B.hBox
    [ shakeViewport uiState
    , B.hLimitPercent 35 $
      B.vBox
      [ B.vLimit 10 $ shakeProgressBar uiState
      , fileWatcherViewport uiState
      ]
    ]
  ]

shakeProgressBar :: UIState -> B.Widget UIRef
shakeProgressBar UIState{uiShakeProgress} =
  B.borderWithLabel (B.str "Shake progress") $
  B.padRight (B.Pad 1) $
  B.padLeft (B.Pad 1) $
  B.vBox
  [ B.progressBar Nothing completionPercentage
  , B.strWrap $ "Skipped: " <> maybe "N" (show . countSkipped) uiShakeProgress
  , B.strWrap $ "Built: " <> maybe "N" (show . countBuilt) uiShakeProgress
  , B.strWrap $ "Unknown: " <> maybe "N" (show . countUnknown) uiShakeProgress
  , B.strWrap $ "Todo: " <> maybe "N" (show . countTodo) uiShakeProgress
  ]
  where
    completionPercentage = 0.5

handleBrickEvent :: (s ~ UIState, n ~ UIRef, e ~ UIEvent)
                 => s
                 -> B.BrickEvent n e
                 -> B.EventM n (B.Next s)
handleBrickEvent uiState@UIState{..} brickEvent = case brickEvent of
  B.AppEvent evt -> case evt of
    EvtFileWatcherStarted t ->
      B.continue $ uiState{uiFileWatcherBuffer=fllAdd (t, "File watcher starter") uiFileWatcherBuffer}
    EvtFileChanged fsEvt -> do
      let fp = makeRelative uiAbsoluteSourceDir $ Notify.eventPath fsEvt
          t = uiToLocalTime $ Notify.eventTime fsEvt
      B.continue $ uiState{uiFileWatcherBuffer=fllAdd (t, fp) uiFileWatcherBuffer}
    EvtShakeOutput verbosity str -> do
      B.continue $ uiState{uiShakeBuffer=fllAdd str uiShakeBuffer}
    EvtUpdateShakeProgress progress -> do
      B.continue $ uiState{uiShakeProgress=Just progress}
  B.VtyEvent evt -> case evt of
    V.EvKey k ms -> case (V.MCtrl `elem` ms) of
      True -> case k of
        V.KChar 'c' -> B.halt uiState
        V.KChar 'C' -> B.halt uiState
        _ -> B.continue uiState
      False -> B.continue uiState
    _ -> B.continue uiState
  _ -> B.continue uiState

yellowText :: B.AttrName
yellowText = B.attrName "yellowText"

attrFileChangedMarker :: B.AttrName
attrFileChangedMarker = B.attrName "fileChangedMarker"

brickAttrMap :: B.AttrMap
brickAttrMap = B.attrMap (B.fg B.white) $
  [ (attrFileChangedMarker, B.fg B.brightYellow)
  , (B.progressIncompleteAttr, B.bg B.white)
  , (B.progressCompleteAttr, B.bg B.green)
  ]

shakeViewport :: UIState -> B.Widget UIRef
shakeViewport UIState{uiShakeBuffer} =
  B.borderWithLabel (B.str "Build output") $
  B.viewport ShakeViewport B.Vertical $
  B.vBox $ (flip DL.map) (fllElements uiShakeBuffer) B.strWrap

fileWatcherViewport :: UIState -> B.Widget UIRef
fileWatcherViewport UIState{uiFileWatcherBuffer} =
  B.borderWithLabel (B.str "File watcher") $
  B.vBox $ (flip DL.map) (fllElements uiFileWatcherBuffer) $ \(t, s) ->
  B.hBox [ B.padRight (B.Pad 1) $
           B.withAttr attrFileChangedMarker $
           B.str $
           formatTime defaultTimeLocale "%T" t
         , B.strWrap s
         ]

handleFileChangeEvent :: B.BChan UIEvent -> MVar () -> Notify.Event -> IO ()
handleFileChangeEvent eventChan buildMvar evt = do
  -- TODO: non-blocking?
  B.writeBChan eventChan $ EvtFileChanged evt
  putMVar buildMvar ()


runFileWatcher :: UIOpts -> B.BChan UIEvent -> IO ()
runFileWatcher UIOpts{..} eventChan = do
  buildMVar <- newMVar ()
  withAsync (buildSupervisor buildCmd buildMVar) $ \_ -> do
    Notify.withManagerConf watchConfig $ \mgr -> do
      void $ Notify.watchTree mgr optSourceDir isRelevantEvent (handleFileChangeEvent eventChan buildMVar)
      t <- getCurrentLocalTime
      B.writeBChan eventChan $ EvtFileWatcherStarted t
      forever $ threadDelay 1000000
  where
    watchConfig = Notify.defaultConfig { Notify.confDebounce = Notify.Debounce 1 }
    isRelevantEvent evt = not $ ".#" `DL.isPrefixOf` (takeFileName $ Notify.eventPath evt)
    myShakeOptions = shakeOptions
                     { shakeThreads = 0
                     , shakeLint = Just LintBasic
                     , shakeChange = ChangeDigest
                     , shakeOutput = brickShakeOutput
                     -- , shakeCommandOptions = [EchoStdout False, EchoStderr False]
                     , shakeProgress = myShakeProgress
                     }
    -- TODO: colorize the output based on verbosity
    brickShakeOutput verbosity str = B.writeBChan eventChan $ EvtShakeOutput verbosity str
    buildCmd = optBuild myShakeOptions
    myShakeProgress pollProgress = forever $ do
      progress <- pollProgress
      B.writeBChan eventChan $ EvtUpdateShakeProgress progress
      threadDelay 1000000

buildSupervisor :: IO () -> MVar () -> IO ()
buildSupervisor buildCmd buildMVar = forever $ do
  takeMVar buildMVar
  (try buildCmd) >>= \case
    Left (e :: SomeException) -> putStrLn $ "Error in running the build: " <> show e
    Right _ -> pure ()

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime
