module Log where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Log.Logger (Priority(DEBUG), errorM, infoM, warningM, setLevel, updateGlobalLogger)


setup :: IO ()
setup = do
  updateGlobalLogger "app" $ setLevel DEBUG
  hSetBuffering stdout LineBuffering


namespace :: String
namespace = "app"


error :: String -> IO ()
error str = errorM namespace $ "ERROR | " ++ str


info :: String -> IO ()
info str = infoM namespace $ "INFO | " ++ str


warning :: String -> IO ()
warning str = warningM namespace $ "WARNING | " ++ str
