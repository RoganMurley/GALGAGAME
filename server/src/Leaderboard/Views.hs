module Leaderboard.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Leaderboard.Apps qualified as Leaderboard
import Network.HTTP.Types.Status (ok200)
import User.Apps qualified as User
import Web.Scotty (ActionM, json, status)
import Web.Scotty.Cookie (getCookies)

leaderboardView :: ConnectInfoConfig -> ActionM ()
leaderboardView config = do
  cookies <- getCookies
  user <- lift $ runApp config $ User.getUserFromCookies cookies ""
  leaderboard <- lift $ runApp config $ Leaderboard.loadWithMe user Nothing
  json leaderboard
  status ok200
