module Metrics where

import Config (App, getDatadogCreds)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Log
import Network.Datadog (Metric (..), MetricPoints (..), sendMetrics, series, withDatadog)

incr :: Text -> App ()
incr name = do
  timestamp <- liftIO $ getPOSIXTime
  let metric = Metric name (Counter [(timestamp, 1)]) Nothing []
  mCreds <- getDatadogCreds
  case mCreds of
    Just creds ->
      liftIO $ withDatadog creds $ \client -> sendMetrics client (series [metric])
    _ ->
      Log.info $ cs $ encode metric
