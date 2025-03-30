module Replay.Final where

import Data.Aeson (ToJSON (..), object, (.=))
import GameState (PlayState)
import Mirror (Mirror (..))
import Model (Model)
import Player (WhichPlayer (..))
import Replay.Active qualified as Active
import ResolveData (ResolveData (..))

data Replay = Replay Active.Replay PlayState
  deriving (Show, Eq)

instance ToJSON Replay where
  toJSON (Replay (Active.Replay initial res (pa, _) (pb, _)) final) =
    object
      [ "list" .= res,
        "initial" .= initial,
        "final" .= final,
        "pa" .= pa,
        "pb" .= pb
      ]

instance Mirror Replay where
  mirror (Replay active final) =
    Replay (mirror active) (mirror final)

finalise :: Active.Replay -> PlayState -> Replay
finalise = Replay

getReplayUser :: WhichPlayer -> Replay -> Active.ReplayUser
getReplayUser which (Replay (Active.Replay _ _ pa pb) _) =
  case which of
    PlayerA ->
      pa
    PlayerB ->
      pb

getRes :: Replay -> [ResolveData]
getRes (Replay (Active.Replay _ res _ _) _) = res

getInitial :: Replay -> Model
getInitial (Replay (Active.Replay initial _ _ _) _) = initial
