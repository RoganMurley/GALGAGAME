module Outcome where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), object)
import Data.Text (Text)
import GameState (PlayState)
import Model (Model)
import Player (WhichPlayer)
import ResolveData (ResolveData)
import Username (Username)

import qualified Replay.Final as Final


data Outcome =
    Sync
  | SaveReplay Final.Replay
  | Encodable Encodable
  deriving (Eq, Show)


data Encodable =
    Chat Username Text
  | Hover WhichPlayer HoverState (Damage, Damage)
  | Resolve [ResolveData] Model PlayState (Maybe WhichPlayer)
  deriving (Eq, Show)

data HoverState = HoverHand Index | HoverStack Index | NoHover
  deriving (Eq, Show)

type Index = Int
type Damage = Int


instance ToJSON Encodable where
  toJSON (Chat name msg) =
    object
      [ "name" .= name
      , "msg"  .= msg
      ]
  toJSON (Hover _ hoverState _) =
    toJSON hoverState
  toJSON (Resolve res initial state _) =
    object
      [ "list"    .= res
      , "initial" .= initial
      , "final"   .= state
      ]


instance ToJSON HoverState where
  toJSON (HoverHand index) =
    object [ "hand" .= index ]
  toJSON (HoverStack index) =
    object [ "stack" .= index ]
  toJSON NoHover =
    Null


instance FromJSON HoverState where
  parseJSON (Object v) =
    (HoverHand <$> v .: "hand") <|> (HoverStack <$> v .: "stack")
  parseJSON Null = pure NoHover
  parseJSON _ = fail "Not a valid HoverState"
