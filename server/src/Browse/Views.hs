module Browse.Views where

import Browse.Apps (loadGames)
import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Network.HTTP.Types.Status (ok200)
import Web.Scotty (ActionM, json, param, status)

browseView :: ConnectInfoConfig -> ActionM ()
browseView config = do
  games <- lift . runApp config $ loadGames
  json games
  status ok200
