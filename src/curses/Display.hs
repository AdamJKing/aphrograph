{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display where

import           Control.Monad                  ( MonadPlus
                                                , msum
                                                )
import           Axis
import           Logging
import           Data.Ord
import           Data.List
import qualified UI.NCurses                    as NC
import           Streaming
import qualified Streaming.Prelude             as S
import           Graphite
import           Control.Monad.Base
import           Control.Monad.Log
import           Data.Text.Prettyprint.Doc

newtype ComponentT m n = ComponentT {
  update :: n -> m ()
}

newtype DisplayT m n = DisplayT {
  windows :: [ComponentT m n]
}

instance MonadBase NC.Curses NC.Curses where
  liftBase = id

createDisplay
  :: (MonadLog (Doc String) m, MonadBase NC.Curses m)
  => m (DisplayT m [DataPoint])
createDisplay = do
  (height, width)      <- liftBase NC.screenSize
  graphWindow          <- liftBase $ NC.newWindow (height - 5) (width - 5) 0 3
  verticalAxisWindow   <- liftBase $ NC.newWindow height 5 0 0
  horizontalAxisWindow <- liftBase $ NC.newWindow 5 width (height - 5) 0
  let msg =
        "Created display. width=" ++ show width ++ ", height=" ++ show height
  logMessage $ pretty msg
  return DisplayT
    { windows = [ verticalAxisComponent verticalAxisWindow
                , horizontalAxisComponent horizontalAxisWindow
                ]
    }

verticalAxisComponent
  :: (MonadBase NC.Curses m) => NC.Window -> ComponentT m [DataPoint]
verticalAxisComponent verticalAxisWindow = ComponentT {update = updateAxis}
 where
  updateAxis data' =
    liftBase
      . NC.updateWindow verticalAxisWindow
      . renderVerticalAxis
      $ verticalAxisTransformer data'

horizontalAxisComponent
  :: (MonadBase NC.Curses m) => NC.Window -> ComponentT m [DataPoint]
horizontalAxisComponent horizontalAxisWindow = ComponentT {update = updateAxis}
 where
  updateAxis data' =
    liftBase
      . NC.updateWindow horizontalAxisWindow
      . renderVerticalAxis
      $ horizontalAxisTransformer data'

updateDisplay :: (Monad m) => data' -> DisplayT m data' -> m ()
updateDisplay data' display = mapM_ (`update` data') (windows display)
