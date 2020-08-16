module Development.Shake.UI.Types where

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
import qualified System.FSNotify as Notify
import Data.Time
import Data.Time.Format
import Development.Shake
import qualified Data.List as DL

-- |
data UIOpts = UIOpts
  { optSourceDir :: !FilePath
  , optBuild :: !(ShakeOptions -> IO ())
  }

-- |
data UIState = UIState
  { uiFileWatcherBuffer :: !(FixedLenthList (LocalTime, String))
  , uiShakeBuffer :: !(FixedLenthList String)
  , uiAbsoluteSourceDir :: !FilePath
  , uiToLocalTime :: !(UTCTime -> LocalTime)
  , uiShakeProgress :: !(Maybe Progress)
  }

-- |
data UIEvent = EvtFileWatcherStarted !LocalTime
             | EvtFileChanged !Notify.Event
             | EvtShakeOutput !Verbosity !String
             | EvtUpdateShakeProgress !Progress
             deriving (Eq, Show)

-- |
data UIRef = FileWatcherViewport
           | ShakeViewport
           deriving (Eq, Show, Ord)

data FixedLenthList a = FixedLenthList
  { fllLength :: !Int
  , fllElements :: ![a]
  } deriving (Eq, Show)

fllAdd :: a -> FixedLenthList a -> FixedLenthList a
fllAdd e (FixedLenthList n elems)=
  let newList = elems ++ [e]
      extra = (DL.length newList) - n
  in if extra > 0
     then FixedLenthList n $ DL.drop extra newList
     else FixedLenthList n newList
