module Replay.Views where

import Config (ConnectInfoConfig, runApp)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object)
import Data.String.Conversions (cs)
import qualified Log
import qualified Metrics
import Network.HTTP.Types.Status (notFound404, ok200)
import qualified Replay.Apps as Replay
import Text.Printf (printf)
import qualified User.Apps as User
import qualified User.User as User
import Web.Scotty (ActionM, json, param, raw, setHeader, status)
import Web.Scotty.Cookie (getCookies)

replayView :: ConnectInfoConfig -> ActionM ()
replayView config = do
  cookies <- getCookies
  replayId <- param "replayId"
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
