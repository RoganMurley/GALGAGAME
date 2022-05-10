module Replay.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status (notFound404, ok200)
import qualified Replay.Final as Replay
import Web.Scotty (ActionM, json, param, status)

replayView :: ConnectInfoConfig -> ActionM ()
replayView config = do
  replayId <- param "replayId"
  mReplay <- lift . runApp config $ Replay.load replayId
  case mReplay of
    Just replay -> do
      json $ object ["replay" .= replay]
      status ok200
    Nothing -> do
      json $ object []
      status notFound404
