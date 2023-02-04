{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Card where

import Control.DeepSeq (NFData (..))
import {-# SOURCE #-} qualified DSL.Alpha.DSL as Alpha
import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding, object, (.=))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, toLower, toUpper)
import GHC.Generics (Generic)
import Player (WhichPlayer (..))
import Prelude hiding (init)

instance Eq Card where
  a == b =
    (card_aspect a == card_aspect b) && (card_suit a == card_suit b) && (card_statuses a == card_statuses b)

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
    card_fakeEff :: Maybe (WhichPlayer -> Beta.Program ()),
    card_playEff :: Card -> WhichPlayer -> Beta.Program Card,
    card_init :: Card -> WhichPlayer -> Alpha.Program Card,
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
  | Devil
  | Trick
  | TrickDisguised Aspect
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
    Devil,
    Trick,
    Strange
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
newCard aspect suit desc eff =
  Card
    { card_aspect = aspect,
      card_suit = suit,
      card_desc = desc,
      card_eff = eff,
      card_fakeEff = Nothing,
      card_playEff = \card _ -> return card,
      card_init = \card _ -> return card,
      card_statuses = [],
      card_related = []
    }

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

addPlayEff :: (Card -> WhichPlayer -> Beta.Program Card) -> Card -> Card
addPlayEff playEff card = card {card_playEff = playEff}

addInit :: (Card -> WhichPlayer -> Alpha.Program Card) -> Card -> Card
addInit init card = card {card_init = init}

removeStatus :: Status -> Card -> Card
removeStatus status card = card {card_statuses = filter (status /=) (card_statuses card)}

removeStatuses :: Card -> Card
removeStatuses card = card {card_statuses = []}

hasStatus :: Status -> Card -> Bool
hasStatus status card = elem status $ card_statuses card

sameCard :: Card -> Card -> Bool
sameCard a b = (card_aspect a == card_aspect b) && (card_suit a == card_suit b)

getFakeEffOrEff :: Card -> (WhichPlayer -> Beta.Program ())
getFakeEffOrEff Card {card_eff, card_fakeEff} = fromMaybe card_eff card_fakeEff

realiseFakeEff :: Card -> Card
realiseFakeEff card = card {card_eff = getFakeEffOrEff card, card_fakeEff = Nothing}
