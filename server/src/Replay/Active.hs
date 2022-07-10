{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Replay.Active where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Life (initMaxLife)
import Mirror (Mirror (..))
import Model (Model (Model), Passes (NoPass), PlayerModel (PlayerModel), miscInit)
import Player (WhichPlayer (PlayerA))
import ResolveData (ResolveData (..))
import qualified Stack
import Util (mkGen)

data Replay = Replay
  { replay_model :: Model,
    replay_res :: [ResolveData],
    replay_pa :: ReplayUser,
    replay_pb :: ReplayUser
  }
  deriving (Eq, Generic, NFData, Show)

type DisplayUsername = Text

type UserId = Maybe Int64

type ReplayUser = (DisplayUsername, UserId)

instance Mirror Replay where
  mirror (Replay m ms pa pb) = Replay (mirror m) (mirror <$> ms) pb pa

add :: Replay -> [ResolveData] -> Replay
add (Replay m xs pa pb) ys = Replay m (xs ++ ys) pa pb

init :: Model -> ReplayUser -> ReplayUser -> Replay
init model pa pb =
  Replay
    { replay_model = model,
      replay_res = [],
      replay_pa = pa,
      replay_pb = pb
    }

null :: Replay
null =
  Replay
    { replay_model = Model PlayerA Stack.init (PlayerModel [] [] 0 initMaxLife) (PlayerModel [] [] 0 initMaxLife) NoPass (mkGen 0) 0 False miscInit,
      replay_res = [],
      replay_pa = ("", Nothing),
      replay_pb = ("", Nothing)
    }
