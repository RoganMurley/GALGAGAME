module Card where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toLower, toUpper)
import Player (WhichPlayer(..))

import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta


instance Eq Card where
  (Card a1 s1 d1 _) == (Card a2 s2 d2 _) =
    a1 == a2 && s1 == s2 && d1 == d2


instance Show Card where
  show card = cs $ cardName card


instance ToJSON Card where
  toJSON card =
    object
      [ "name"     .= cardName card
      , "desc"     .= card_desc card
      , "imageURL" .= cardImgUrl (card_aspect card) (card_suit card)
      ]


data Card = Card
  { card_aspect :: Aspect
  , card_suit   :: Suit
  , card_desc   :: Text
  , card_eff    :: WhichPlayer -> Beta.Program ()
  }


data Suit
  = Sword
  | Wand
  | Grail
  | Coin
  | OtherSuit Text
  deriving (Eq, Show)


data Aspect
  = Basic
  | Blaze
  | Heaven
  | Duality
  | Shroom
  | Blood
  | Mirage
  | Mirror
  | Alchemy
  | Crown
  | Morph
  | Strange
  | OtherAspect Text
  deriving (Eq, Show)



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
cardName (Card{ card_aspect, card_suit }) =
  case card_aspect of
    Basic ->
      toUpper $ suitText card_suit
    _ ->
      toUpper $ aspectText card_aspect <> " " <> suitText card_suit


cardImgUrl :: Aspect -> Suit -> Text
cardImgUrl aspect suit =
  toLower $ "cards/" <> aspectText aspect <> "/" <> suitText suit <> ".png"
