{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Card where

import Control.DeepSeq (NFData (..))
import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding, object, (.=))
import Data.String.Conversions (cs)
import Data.Text (Text, toLower, toUpper)
import GHC.Generics (Generic)
import Player (WhichPlayer (..))

instance Eq Card where
  (Card a1 s1 _ _ st1 r1) == (Card a2 s2 _ _ st2 r2) =
    a1 == a2 && s1 == s2 && st1 == st2 && r1 == r2

instance Show Card where
  show card = cs $ cardName (card_aspect card) (card_suit card)

instance ToJSON Card where
  toJSON card =
    object
      [ "name" .= cardName (card_aspect card) (card_suit card),
        "desc" .= card_desc card,
        "imageURL" .= cardImgUrl (card_aspect card) (card_suit card),
        "statuses" .= card_statuses card,
        "related" .= card_related card
      ]

data Card = Card
  { card_aspect :: Aspect,
    card_suit :: Suit,
    card_desc :: Text,
    card_eff :: WhichPlayer -> Beta.Program (),
    card_statuses :: [Status],
    card_related :: [Related]
  }
  deriving (Generic, NFData)

data Suit
  = Sword
  | Wand
  | Cup
  | Coin
  | OtherSuit Text
  deriving (Eq, Generic, NFData, Ord, Show)

data Aspect
  = Angel
  | Water
  | Fire
  | Shroom
  | Gold
  | Mirror
  | Duality
  | Blood
  | Clay
  | Plastic
  | Fever
  | Void
  | Glass
  | Eye
  | Demon
  | Strange
  | OtherAspect Text
  deriving (Eq, Generic, NFData, Ord, Show)

instance ToJSON Related where
  toJSON related =
    object
      [ "name" .= cardName (related_aspect related) (related_suit related),
        "desc" .= related_desc related,
        "imageURL" .= cardImgUrl (related_aspect related) (related_suit related)
      ]

data Related = Related
  { related_aspect :: Aspect,
    related_suit :: Suit,
    related_desc :: Text
  }
  deriving (Eq, Show, Generic, NFData)

suitText :: Suit -> Text
suitText suit =
  case suit of
    OtherSuit text ->
      text
    _ ->
      cs $ show suit

aspectText :: Aspect -> Text
aspectText aspect =
  case aspect of
    OtherAspect text ->
      text
    _ ->
      cs $ show aspect

cardName :: Aspect -> Suit -> Text
cardName aspect suit =
  toUpper $ aspectText aspect <> " " <> suitText suit

cardImgUrl :: Aspect -> Suit -> Text
cardImgUrl aspect suit =
  toLower $ "cards/" <> aspectText aspect <> "/" <> suitText suit <> ".png"

allAspects :: [Aspect]
allAspects =
  [ Angel,
    Water,
    Fire,
    Shroom,
    Gold,
    Mirror,
    Duality,
    Blood,
    Clay,
    Plastic,
    Fever,
    Void,
    Glass,
    Eye,
    Strange,
    Demon
  ]

data Status
  = StatusEcho
  | StatusBlighted
  | StatusFragile
  | StatusBonusDamage Int
  deriving (Eq, Generic, NFData, Ord, Show)

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions

newCard :: Aspect -> Suit -> Text -> (WhichPlayer -> Beta.Program ()) -> Card
newCard aspect suit desc eff = Card aspect suit desc eff [] []

addStatus :: Status -> Card -> Card
addStatus status card =
  let statuses = card_statuses card
   in if length statuses < 6
        then card {card_statuses = status : statuses}
        else card

addRelated :: Card -> Card -> Card
addRelated relatedCard card =
  card {card_related = related : card_related card}
  where
    related :: Related
    related =
      Related
        { related_aspect = card_aspect relatedCard,
          related_suit = card_suit relatedCard,
          related_desc = card_desc relatedCard
        }

removeStatus :: Status -> Card -> Card
removeStatus status card = card {card_statuses = filter (status /=) (card_statuses card)}

removeStatuses :: Card -> Card
removeStatuses card = card {card_statuses = []}

hasStatus :: Status -> Card -> Bool
hasStatus status card = elem status $ card_statuses card
