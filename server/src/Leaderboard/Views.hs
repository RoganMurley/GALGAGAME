module Leaderboard.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Leaderboard.Apps (loadLeaderboard)
import Network.HTTP.Types.Status (ok200)
import Web.Scotty (ActionM, json, status)

leaderboardView :: ConnectInfoConfig -> ActionM ()
leaderboardView config = do
  leaderboard <- lift $ runApp config loadLeaderboard
  json leaderboard
  status ok200