module App.Logging where

import Control.Concurrent (Chan, getChanContents)
import Control.Concurrent.Lifted (fork)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Logger (LogLine, monadLoggerLog, runFileLoggingT)

fileWritingLogChan :: Chan LogLine -> FilePath -> IO ()
fileWritingLogChan logChan logFile =
  void
    . fork
    . forever
    $ do
      logLines <- liftBase (getChanContents logChan)
      when (not (null logLines)) $
        runFileLoggingT logFile $
          forM_ logLines $ \(loc, src, lvl, msg) ->
            monadLoggerLog loc src lvl msg
