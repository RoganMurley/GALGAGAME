module Log where

import Config (App, getLoggerChan)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Concurrent.Lifted (ThreadId, fork)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Log.Logger (Priority (..), debugM, errorM, infoM, setLevel, updateGlobalLogger, warningM)

setup :: IO ()
setup = do
  updateGlobalLogger "app" $ setLevel DEBUG
  hSetBuffering stdout LineBuffering

forkLogger :: Chan (Priority, String) -> IO ThreadId
forkLogger chan =
  fork $
    forever $
      liftIO $ do
        (priority, str) <- readChan chan
        let logger = loggerFromPriority priority
        logger str

namespace :: String
namespace = "app"

loggerFromPriority :: Priority -> (String -> IO ())
loggerFromPriority DEBUG = debugIO
loggerFromPriority INFO = infoIO
loggerFromPriority WARNING = warningIO
loggerFromPriority _ = errorIO

errorIO :: String -> IO ()
errorIO str = errorM namespace $ "ERROR | " ++ str

error :: String -> App ()
error str = do
  chan <- getLoggerChan
  liftIO $ writeChan chan (ERROR, str)

debugIO :: String -> IO ()
debugIO str = debugM namespace $ "DEBUG | " ++ str

debug :: String -> App ()
debug str = do
  chan <- getLoggerChan
  liftIO $ writeChan chan (DEBUG, str)

infoIO :: String -> IO ()
infoIO str = infoM namespace $ "INFO | " ++ str

info :: String -> App ()
info str = do
  chan <- getLoggerChan
  liftIO $ writeChan chan (INFO, str)

warningIO :: String -> IO ()
warningIO str = warningM namespace $ "WARNING | " ++ str

warning :: String -> App ()
warning str = do
  chan <- getLoggerChan
  liftIO $ writeChan chan (WARNING, str)
