module Replay where

import CardAnim (CardAnim)
import Mirror (Mirror(..))
import Model (Model(Model), Passes(NoPass), PlayerModel(PlayerModel))
import ModelDiff (ModelDiff)
import Player (WhichPlayer(PlayerA))
import StackCard (StackCard)
import Util (mkGen)


newtype Replay = Replay (Model, [(ModelDiff, Maybe CardAnim, Maybe StackCard)])
  deriving (Show, Eq)


instance Mirror Replay where
  mirror (Replay (m, ms)) =
    Replay (mirror m, (\(x, y, z) -> (mirror x, mirror y, mirror z)) <$> ms)


add :: Replay -> [(ModelDiff, Maybe CardAnim, Maybe StackCard)] -> Replay
add (Replay (m, xs)) ys = Replay (m, xs ++ ys)


init :: Model -> Replay
init model = Replay (model, [])


null :: Replay
null = Replay (Model PlayerA [] (PlayerModel [] [] 0) (PlayerModel [] [] 0) NoPass (mkGen 0), [])
