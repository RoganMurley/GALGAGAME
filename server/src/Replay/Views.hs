module Replay.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Data.Map qualified as Map
import Data.String.Conversions (cs)
import Log qualified
import Metrics qualified
import Network.HTTP.Types.Status (notFound404, ok200)
import Replay.Apps qualified as Replay
import Text.Printf (printf)
import User.Apps qualified as User
import User.User qualified as User
import Web.Scotty (ActionM, json, pathParam, raw, setHeader, status)
import Web.Scotty.Cookie (getCookies)

replayView :: ConnectInfoConfig -> ActionM ()
replayView config = do
  cookies <- Map.fromList <$> getCookies
  replayId <- pathParam "replayId"
  mReplay <- lift . runApp config $ do
    username <- User.getUsername <$> User.getUserFromCookies cookies ""
    Metrics.incr "request.replay"
    Log.info $ printf "<%s>: watching replay %s" username (show replayId)
    Replay.load replayId
  case mReplay of
    Just replay -> do
      raw $ cs replay
      setHeader "Content-Type" "application/json"
      status ok200
    Nothing -> do
      json $ object []
      status notFound404
