module Replay.Active where

import Data.Text (Text)
import Mirror (Mirror(..))
import Model (Model(Model), Passes(NoPass), PlayerModel(PlayerModel))
import Player (WhichPlayer(PlayerA))
import ResolveData (ResolveData(..))
import Util (mkGen)


data Replay = Replay Model [ResolveData] Text Text
  deriving (Show, Eq)


instance Mirror Replay where
  mirror (Replay m ms pa pb) = Replay (mirror m) (mirror <$> ms) pb pa


add :: Replay -> [ResolveData] -> Replay
add (Replay m xs pa pb) ys = Replay m (xs ++ ys) pa pb


init :: Model -> Text -> Text -> Replay
init model pa pb = Replay model [] pa pb


null :: Replay
null =
  Replay
    (Model PlayerA [] (PlayerModel [] [] 0) (PlayerModel [] [] 0) NoPass (mkGen 0))
    []
    ""
    ""
