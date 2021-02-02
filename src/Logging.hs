module Logging where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.IO (stdout, stderr)
import GHC.IO.Handle

setupLogging :: IO ()
setupLogging = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

mkLog :: forall m. MonadIO m => Text -> (Text -> m (), Text -> m ())
mkLog name = (logInfo, logError)
 where
  logInfo = liftIO . TIO.putStrLn . (wrappedName <>) . ("INFO: " <>)
  logError = liftIO . TIO.hPutStrLn stderr . (wrappedName <>) . ("ERROR: " <>)
  wrappedName = mconcat ["[", name, "] "]
