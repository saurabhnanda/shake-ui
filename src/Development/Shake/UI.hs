{-# LANGUAGE PartialTypeSignatures #-}

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
import Data.Maybe (fromMaybe, maybe)

mkUIOpts :: Maybe (B.BChan evt)
         -> (UIEvent -> evt)
         -> (ShakeOptions -> IO ())
         -> (UIOpts evt -> UIOpts evt)
         -> IO (UIOpts evt)
mkUIOpts mEventChan constr buildFn overrideFn = do
  eventChan <- maybe (B.newBChan 100) pure mEventChan
  pure $ overrideFn $ UIOpts
    { optSourceDir = "src"
    , optBuild = buildFn
    , optEventChan = eventChan
    , optEventConstr = constr
    }

mkInitialUIState :: UIOpts evt -> IO UIState
mkInitialUIState UIOpts{..} = do
  tz <- getCurrentTimeZone
  let uiFileWatcherBuffer = FixedLenthList { fllLength = 10, fllElements = [] }
      uiShakeBuffer = FixedLenthList { fllLength = 1000, fllElements = [] }
      uiToLocalTime = utcToLocalTime tz
      uiShakeProgress = Nothing
  uiAbsoluteSourceDir <- makeAbsolute optSourceDir
  pure UIState{..}

-- defaultMain :: UIOpts UIEvent -> IO ()
-- defaultMain uiOpts@UIOpts{..} = do
--   tz <- getCurrentTimeZone
--   let buildVty = V.mkVty V.defaultConfig
--       brickApp :: B.App UIState UIEvent UIRef
--       brickApp = B.App
--                  { B.appDraw = renderBrickApp
--                  , B.appChooseCursor = (const $ const Nothing)
--                  , B.appHandleEvent = handleBrickEvent
--                  , B.appStartEvent = \s -> do
--                      B.vScrollToEnd $ B.viewportScroll FileWatcherViewport
--                      pure s
--                  , B.appAttrMap = brickAttrMap
--                  }
--   uiState <- mkInitialUIState uiOpts
--   initialVty <- buildVty
--   withAsync (runFileWatcher uiOpts optEventChan) $ \_ -> do
--     void $ B.customMain initialVty buildVty (Just optEventChan) brickApp uiState


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
  , B.strWrap $ "isFailure: " <> maybe "N" (show . isFailure) uiShakeProgress
  ]
  where
    completionPercentage = case uiShakeProgress of
      Nothing -> 0.0
      Just Progress{..} ->
        -- NOTE: https://github.com/jtdaugherty/brick/issues/281
        let p = (fromIntegral $ countSkipped + countBuilt) / (fromIntegral $ countTodo + countUnknown + countSkipped + countBuilt)
        in if p==1.00
           then p
           else 0.95

handleBrickEvent :: s
                 -> B.BrickEvent n e
                 -> (e -> B.EventM n (B.Next s))
                 -> B.EventM n (B.Next s)
handleBrickEvent state brickEvent callback = case brickEvent of
  B.AppEvent evt ->
    callback evt
  B.VtyEvent evt -> case evt of
    V.EvKey k ms -> case (V.MCtrl `elem` ms) of
      True -> case k of
        V.KChar 'c' -> B.halt state
        V.KChar 'C' -> B.halt state
        _ -> B.continue state
      False -> B.continue state
    _ -> B.continue state
  _ -> B.continue state

handleUIEvent :: (n ~ UIRef)
              => (UIState -> state)
              -> UIState
              -> UIEvent
              -> B.EventM n (B.Next state)
handleUIEvent updateState uiState@UIState{..} evt =
  case evt of
    EvtFileWatcherStarted t ->
      B.continue $ updateState $
      uiState{uiFileWatcherBuffer=fllAdd (t, "File watcher starter") uiFileWatcherBuffer}
    EvtFileChanged fsEvt -> do
      let fp = makeRelative uiAbsoluteSourceDir $ Notify.eventPath fsEvt
          t = uiToLocalTime $ Notify.eventTime fsEvt
      B.continue $
        updateState $
        uiState{uiFileWatcherBuffer=fllAdd (t, fp) uiFileWatcherBuffer}
    EvtShakeOutput verbosity str ->
      B.continue $ updateState
      uiState{uiShakeBuffer=fllAdd (show verbosity <> ": " <> str) uiShakeBuffer}
    EvtUpdateShakeProgress progress ->
      B.continue $ updateState $
      uiState{uiShakeProgress=Just progress}
    EvtShakeTrace k c b ->
      B.continue $ updateState $
      uiState{uiShakeBuffer = fllAdd (k <> " / " <> c <> " / " <> show b) uiShakeBuffer}

yellowText :: B.AttrName
yellowText = B.attrName "yellowText"

attrFileChangedMarker :: B.AttrName
attrFileChangedMarker = B.attrName "fileChangedMarker"

brickAttrMap :: UIState -> B.AttrMap
brickAttrMap UIState{uiShakeProgress} = B.attrMap (B.fg B.white) $
  [ (attrFileChangedMarker, B.fg B.brightYellow)
  , (B.progressIncompleteAttr, B.bg B.white)
  , (B.progressCompleteAttr, B.bg progressColor)
  ]
  where
    progressColor =
      fromMaybe B.green $
      -- next line is in the Maybe monad
      uiShakeProgress >>= isFailure >>= (const $ pure B.red)

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

handleFileChangeEvent :: UIOpts evt
                      -> B.BChan evt
                      -> MVar ()
                      -> Notify.Event
                      -> IO ()
handleFileChangeEvent UIOpts{optEventConstr} eventChan buildMvar evt = do
  -- TODO: non-blocking?
  B.writeBChan eventChan $ optEventConstr $ EvtFileChanged evt
  putMVar buildMvar ()


runFileWatcher :: UIOpts evt -> B.BChan evt -> IO ()
runFileWatcher uiOpts@UIOpts{..} eventChan = do
  buildMVar <- newMVar ()
  withAsync (buildSupervisor buildCmd buildMVar) $ \_ -> do
    Notify.withManagerConf watchConfig $ \mgr -> do
      void $ Notify.watchTree mgr optSourceDir isRelevantEvent (handleFileChangeEvent uiOpts eventChan buildMVar)
      t <- getCurrentLocalTime
      B.writeBChan eventChan $ optEventConstr $ EvtFileWatcherStarted t
      forever $ threadDelay 1000000
  where
    watchConfig = Notify.defaultConfig { Notify.confDebounce = Notify.Debounce 1 }
    isRelevantEvent evt = not $ ".#" `DL.isPrefixOf` (takeFileName $ Notify.eventPath evt)
    myShakeOptions = shakeOptions
                     { shakeThreads = 0
                     , shakeLint = Just LintBasic
                     , shakeChange = ChangeDigest
                     , shakeOutput = brickShakeOutput
                     , shakeCommandOptions = [EchoStdout False, EchoStderr False]
                     , shakeProgress = myShakeProgress
                     -- , shakeReport = ["/tmp/shake-report.json", "/tmp/shake-report.html"]
                     , shakeTrace = brickShakeTrace
                     }
    -- TODO: colorize the output based on verbosity
    brickShakeOutput verbosity str = B.writeBChan eventChan $ optEventConstr $ EvtShakeOutput verbosity str
    brickShakeTrace k c b = B.writeBChan eventChan $ optEventConstr $ EvtShakeTrace k c b
    buildCmd = optBuild myShakeOptions
    myShakeProgress pollProgress = forever $ do
      progress <- pollProgress
      B.writeBChan eventChan $ optEventConstr $ EvtUpdateShakeProgress progress
      threadDelay 10000

buildSupervisor :: IO () -> MVar () -> IO ()
buildSupervisor buildCmd buildMVar = forever $ do
  takeMVar buildMVar
  (try buildCmd) >>= \case
    Left (e :: SomeException) -> putStrLn $ "Error in running the build: " <> show e
    Right _ -> pure ()

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime
