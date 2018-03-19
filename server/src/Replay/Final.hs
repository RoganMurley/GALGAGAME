module Replay.Final where

import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Hashable (Hashable(hash))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GameState (PlayState)
import Mirror (Mirror(..))

import qualified Database.Redis as R
import qualified Replay.Active as Active


newtype Replay = Replay (Active.Replay, PlayState)
  deriving (Show, Eq)


instance ToJSON Replay where
  toJSON (Replay (Active.Replay (initial, res), final)) =
    object [
      "list"    .= res
    , "initial" .= initial
    , "final"   .= final
    ]


instance Mirror Replay where
  mirror (Replay (active, final)) =
    Replay (mirror active, mirror final)


finalise :: Active.Replay -> PlayState -> Replay
finalise active final = Replay (active, final)


save :: R.Connection -> Replay -> IO Bool
save conn replay = do
  result <- (R.runRedis conn) $ do
    _ <- R.set key value
    R.expire key replayTimeout
  case result of
    Right _ ->
      return True
    Left _ ->
      return False
  where
    value = cs $ encode replay
    key = cs . show $ hash value
    replayTimeout = 3600 * 24 -- 1 day


load :: R.Connection -> Text -> IO (Maybe Text)
load conn replayId = do
  result <- (R.runRedis conn) $ do
    R.get (cs replayId)
  case result of
    Right replay ->
      return $ cs <$> replay
    Left _ ->
      return Nothing
