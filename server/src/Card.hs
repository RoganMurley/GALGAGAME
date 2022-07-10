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
  (Card a1 s1 _ _ st1) == (Card a2 s2 _ _ st2) =
    a1 == a2 && s1 == s2 && st1 == st2

instance Show Card where
  show card = cs $ cardName card

instance ToJSON Card where
  toJSON card =
    object
      [ "name" .= cardName card,
        "desc" .= card_desc card,
        "imageURL" .= cardImgUrl (card_aspect card) (card_suit card),
        "statuses" .= card_statuses card
      ]

data Card = Card
  { card_aspect :: Aspect,
    card_suit :: Suit,
    card_desc :: Text,
    card_eff :: WhichPlayer -> Beta.Program (),
    card_statuses :: [Status]
  }
  deriving (Generic, NFData)

data Suit
  = Sword
  | Wand
  | Grail
  | Coin
  | OtherSuit Text
  deriving (Eq, Generic, NFData, Ord, Show)

data Aspect
  = Heaven
  | Tide
  | Blaze
  | Shroom
  | Alchemy
  | Mirror
  | Duality
  | Blood
  | Crown
  | Morph
  | Myriad
  | Abyss
  | Fever
  | Empty
  | Comet
  | Glass
  | Seer
  | Strange
  | OtherAspect Text
  deriving (Eq, Generic, NFData, Ord, Show)

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

cardName :: Card -> Text
cardName Card {card_aspect, card_suit} =
  toUpper $ aspectText card_aspect <> " " <> suitText card_suit

cardImgUrl :: Aspect -> Suit -> Text
cardImgUrl aspect suit =
  toLower $ "cards/" <> aspectText aspect <> "/" <> suitText suit <> ".png"

allAspects :: [Aspect]
allAspects =
  [ Heaven,
    Tide,
    Blaze,
    Shroom,
    Alchemy,
    Mirror,
    Duality,
    Blood,
    Crown,
    Morph,
    Myriad,
    Abyss,
    Fever,
    Empty,
    Comet,
    Myriad,
    Glass,
    Seer,
    Strange
  ]

wipAspects :: [Aspect]
wipAspects = [Comet, Abyss]

data Status = StatusEcho | StatusBlighted | StatusFragile
  deriving (Eq, Generic, NFData, Ord, Show)

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions

newCard :: Aspect -> Suit -> Text -> (WhichPlayer -> Beta.Program ()) -> Card
newCard aspect suit desc eff = Card aspect suit desc eff []

addStatus :: Status -> Card -> Card
addStatus status card =
  let statuses = card_statuses card
   in if length statuses < 6
        then card {card_statuses = status : card_statuses card}
        else card

removeStatus :: Status -> Card -> Card
removeStatus status card = card {card_statuses = filter (status /=) (card_statuses card)}

removeStatuses :: Card -> Card
removeStatuses card = card {card_statuses = []}

hasStatus :: Status -> Card -> Bool
hasStatus status card = elem status $ card_statuses card
