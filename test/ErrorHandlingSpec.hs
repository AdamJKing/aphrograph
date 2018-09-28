module ErrorHandlingSpec where

import           Test.Hspec
import qualified UI.NCurses                    as NC
import           Control.Monad.Catch
import           ErrorHandling


spec = do
  describe "The MonadMask instance for Curses"
    $ it "obeys the monad mask laws" pending
  describe "The MonadThrow instance for Curses"
    $ it "it obeys the monad throw laws"
    $ let e = (error "test" :: SomeException)
          x = pure () :: NC.Curses ()
      in  NC.runCurses (throwM e >> x) `shouldThrow` anyException
  describe "The MonadCatch instance for Curses"
    $ it "it obeys the monad catch laws"
    $ let e = (error "test" :: NC.CursesException)
          x = pure () :: NC.Curses ()
      in  NC.runCurses (catch (throwM e) dudErrorHandler)
            `shouldReturn` "rescued"
 where
  dudErrorHandler :: NC.CursesException -> NC.Curses String
  dudErrorHandler e = return "rescued"
