module Outcome where

import CardAnim (Damage (..))
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import GameState (PlayState)
import Model (Model)
import Player (WhichPlayer)
import qualified Replay.Final as Final
import ResolveData (ResolveData)

data Outcome
  = Sync
  | SaveReplay Final.Replay
  | HandleExperience (Maybe WhichPlayer)
  | Encodable Encodable
  deriving (Eq, Show)

data Encodable
  = Chat Text Text
  | Hover WhichPlayer HoverState (HoverDamage, HoverDamage)
  | Resolve [ResolveData] Model PlayState (Maybe WhichPlayer)
  | Heartbeat NominalDiffTime
  deriving (Eq, Show)

type Index = Int

data HoverState
  = HoverHand Index
  | HoverOtherHand Index
  | HoverStack Index
  | NoHover
  deriving (Eq, Show)

data HoverDamage = HoverDamage Int | HoverDamageUncertain
  deriving (Eq, Show)

instance ToJSON HoverDamage where
  toJSON (HoverDamage a) = toJSON a
  toJSON (HoverDamageUncertain) = toJSON ("?" :: Text)

instance ToJSON Encodable where
  toJSON (Chat name msg) =
    object
      [ "name" .= name,
        "msg" .= msg
      ]
  toJSON (Hover _ hoverState _) =
    toJSON hoverState
  toJSON (Resolve res initial state _) =
    object
      [ "list" .= res,
        "initial" .= initial,
        "final" .= state
      ]
  toJSON (Heartbeat delta) =
    object ["timeLeft" .= delta]

instance ToJSON HoverState where
  toJSON (HoverHand index) =
    object ["hand" .= index]
  toJSON (HoverOtherHand index) =
    object ["otherHand" .= index]
  toJSON (HoverStack index) =
    object ["stack" .= index]
  toJSON NoHover =
    Null

instance FromJSON HoverState where
  parseJSON (Object v) =
    (HoverHand <$> v .: "hand")
      <|> (HoverOtherHand <$> v .: "otherHand")
      <|> (HoverStack <$> v .: "stack")
  parseJSON Null = pure NoHover
  parseJSON _ = fail "Not a valid HoverState"

damageToHoverDamage :: Damage -> HoverDamage
damageToHoverDamage (DamageCertain a) = HoverDamage a
damageToHoverDamage (DamageUncertain 0) = HoverDamage 0
damageToHoverDamage (DamageUncertain _) = HoverDamageUncertain
