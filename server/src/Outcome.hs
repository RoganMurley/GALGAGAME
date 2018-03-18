module Outcome where

import CardAnim (CardAnim)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import GameState (PlayState)
import Model (Model)
import ModelDiff (ModelDiff)
import Player (WhichPlayer)
import Replay (Replay(Replay))
import StackCard (StackCard)
import Username (Username)


type ExcludePlayer = WhichPlayer


data Outcome =
    Sync
  | SaveReplay Replay PlayState
  | Encodable Encodable
  deriving (Eq, Show)


data Encodable =
    Chat Username Text
  | Hover ExcludePlayer (Maybe Int)
  | PlayReplay Replay PlayState
  | Resolve [(ModelDiff, Maybe CardAnim, Maybe StackCard)] Model PlayState
  deriving (Eq, Show)


instance ToJSON Encodable where
  toJSON (Chat name msg) =
    object [
      "name" .= name
    , "msg"  .= msg
    ]
  toJSON (Hover _ index) =
    toJSON index
  toJSON (PlayReplay (Replay (initial, replayData)) final) =
    toJSON $ Resolve replayData initial final
  toJSON (Resolve res initial state) =
    object [
      "list"    .= res
    , "initial" .= initial
    , "final"   .= state
    ]
