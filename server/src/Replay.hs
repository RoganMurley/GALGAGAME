module Replay where

import CardAnim (CardAnim)
import Data.Aeson (ToJSON(..), encode)
import Data.Hashable (Hashable(hash))
import Data.String.Conversions (cs)
import Mirror (Mirror(..))
import Model (Model(Model), Passes(NoPass), PlayerModel(PlayerModel))
import ModelDiff (ModelDiff)
import Player (WhichPlayer(PlayerA))
import StackCard (StackCard)
import Util (mkGen)

import qualified Database.Redis as R


newtype Replay = Replay (Model, [(ModelDiff, Maybe CardAnim, Maybe StackCard)])
  deriving (Show, Eq)


instance ToJSON Replay where
  toJSON (Replay replay) = toJSON replay


instance Mirror Replay where
  mirror (Replay (m, ms)) =
    Replay (mirror m, (\(x, y, z) -> (mirror x, mirror y, mirror z)) <$> ms)


add :: Replay -> [(ModelDiff, Maybe CardAnim, Maybe StackCard)] -> Replay
add (Replay (m, xs)) ys = Replay (m, xs ++ ys)


init :: Model -> Replay
init model = Replay (model, [])


null :: Replay
null = Replay (Model PlayerA [] (PlayerModel [] [] 0) (PlayerModel [] [] 0) NoPass (mkGen 0), [])


save :: R.Connection -> Replay -> IO Bool
save conn replay = do
  result <- (R.runRedis conn) $ R.set key value
  case result of
    Right _ ->
      return True
    Left _ ->
      return False
  where
    value = cs $ encode replay
    key = cs . show $ hash value
