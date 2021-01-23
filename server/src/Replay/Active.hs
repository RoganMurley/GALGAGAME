module Replay.Active where

import Data.Text (Text)
import Life (initMaxLife)
import Mirror (Mirror(..))
import Model (Model(Model), Passes(NoPass), PlayerModel(PlayerModel))
import Player (WhichPlayer(PlayerA))
import ResolveData (ResolveData(..))
import Util (mkGen)

import qualified Stack


data Replay = Replay
  { replay_model :: Model
  , replay_res   :: [ResolveData]
  , replay_pa    :: Usernames
  , replay_pb    :: Usernames
  } deriving (Show, Eq)


type Usernames = (DisplayUsername, QueryUsername)


type DisplayUsername = Text


type QueryUsername = Text


instance Mirror Replay where
  mirror (Replay m ms pa pb) = Replay (mirror m) (mirror <$> ms) pb pa


add :: Replay -> [ResolveData] -> Replay
add (Replay m xs pa pb) ys = Replay m (xs ++ ys) pa pb


init :: Model -> Usernames -> Usernames -> Replay
init model pa pb =
  Replay
    { replay_model = model
    , replay_res   = []
    , replay_pa    = pa
    , replay_pb    = pb
    }


null :: Replay
null =
  Replay
    { replay_model = Model PlayerA Stack.init (PlayerModel [] [] 0 initMaxLife) (PlayerModel [] [] 0 initMaxLife) NoPass (mkGen 0) 0 False
    , replay_res   = []
    , replay_pa    = ("", "")
    , replay_pb    = ("", "")
    }
