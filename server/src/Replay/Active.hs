module Replay.Active where

import CardAnim (CardAnim)
import Mirror (Mirror(..))
import Model (Model(Model), Passes(NoPass), PlayerModel(PlayerModel))
import ModelDiff (ModelDiff)
import Player (WhichPlayer(PlayerA))
import StackCard (StackCard)
import Username (Username(Username))
import Util (mkGen)


data Replay = Replay Model [(ModelDiff, Maybe CardAnim, Maybe StackCard)] Username Username
  deriving (Show, Eq)


instance Mirror Replay where
  mirror (Replay m ms pa pb) =
    Replay
      (mirror m)
      ((\(x, y, z) -> (mirror x, mirror y, mirror z)) <$> ms)
      pb
      pa


add :: Replay -> [(ModelDiff, Maybe CardAnim, Maybe StackCard)] -> Replay
add (Replay m xs pa pb) ys = Replay m (xs ++ ys) pa pb


init :: Model -> Username -> Username -> Replay
init model pa pb = Replay model [] pa pb


null :: Replay
null =
  Replay
    (Model PlayerA [] (PlayerModel [] [] 0) (PlayerModel [] [] 0) NoPass (mkGen 0))
    []
    (Username "")
    (Username "")
