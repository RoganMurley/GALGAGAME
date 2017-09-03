module Outcome where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)

import GameState (GameState)
import Model (Model)
import Player (WhichPlayer)
import Username (Username)


type ExcludePlayer = WhichPlayer


data Outcome =
    Sync
  | PlayCard ExcludePlayer
  | EndTurn ExcludePlayer
  | Encodable Encodable
  deriving (Eq, Show)


data Encodable =
    Chat Username Text
  | Hover ExcludePlayer (Maybe Int)
  | Resolve [Model] GameState
  deriving (Eq, Show)


instance ToJSON Encodable where
  toJSON (Chat name msg) =
    object [
      "name" .= name
    , "msg"  .= msg
    ]
  toJSON (Hover _ index) =
    toJSON index
  toJSON (Resolve res state) =
    object [
      "list"  .= res
    , "final" .= state
    ]
