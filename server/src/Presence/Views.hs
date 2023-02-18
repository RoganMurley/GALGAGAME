module Presence.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (ok200)
import Presence.Apps as Presence
import Web.Scotty (ActionM, json, status)

view :: ConnectInfoConfig -> ActionM ()
view config = do
  presence <- lift $ runApp config Presence.load
  json presence
  status ok200
