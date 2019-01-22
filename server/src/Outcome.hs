module Outcome where

import CardAnim (CardAnim)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import GameState (PlayState)
import Model (Model)
import ModelDiff (ModelDiff)
import Player (WhichPlayer)
import StackCard (StackCard)
import Username (Username)

import qualified Replay.Final as Final


data Outcome =
    Sync
  | SaveReplay Final.Replay
  | Encodable Encodable
  deriving (Eq, Show)


data Encodable =
    Chat Username Text
  | Hover WhichPlayer (Maybe HandIndex) Damage
  | Resolve [(ModelDiff, Maybe CardAnim, Maybe StackCard)] Model PlayState
  deriving (Eq, Show)

type HandIndex = Int
type Damage = Int


instance ToJSON Encodable where
  toJSON (Chat name msg) =
    object
      [ "name" .= name
      , "msg"  .= msg
      ]
  toJSON (Hover _ index _) =
    toJSON index
  toJSON (Resolve res initial state) =
    object
      [ "list"    .= res
      , "initial" .= initial
      , "final"   .= state
      ]
