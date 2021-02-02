module Logging where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO (stdout, stderr)
import GHC.IO.Handle
import Control.Monad ((<=<))

setupLogging :: IO ()
setupLogging = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

mkLog :: forall m. MonadIO m => Text -> (Text -> m (), Text -> m ())
mkLog name = (logInfo, logError)
 where
  logInfo = liftIO . TIO.putStrLn <=< prefixWithTime . (wrappedName <>) . ("INFO: " <>)
  logError = liftIO . TIO.hPutStrLn stderr <=< prefixWithTime . (wrappedName <>) . ("ERROR: " <>)
  wrappedName = mconcat ["[", name, "] "]
  prefixWithTime str = do
    time <- liftIO $ utcToLocalZonedTime =<< getCurrentTime
    return $ pack (show time) <> " " <> str
