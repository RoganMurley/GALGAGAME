module Card where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import Player (WhichPlayer(..))

import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta


instance Eq Card where
  (Card n1 d1 i1 a1 s1 _) == (Card n2 d2 i2 a2 s2 _) =
    n1 == n2 && d1 == d2 && i1 == i2 && a1 == a2 && s1 == s2


instance Show Card where
  show = cs . card_name


instance ToJSON Card where
  toJSON (Card{ card_name, card_desc, card_img }) =
    object
      [ "name"     .= toUpper card_name
      , "desc"     .= card_desc
      , "imageURL" .= card_img
      ]


data Card = Card
  { card_name   :: Text
  , card_desc   :: Text
  , card_img    :: Text
  , card_aspect :: Aspect
  , card_suit   :: Suit
  , card_eff    :: WhichPlayer -> Beta.Program ()
  }



description :: Card -> Text
description Card{ card_name, card_desc } =
  "(" <> card_name <> ": " <> card_desc <> ")"


data Suit = Sword | Wand | Cup | Coin | OtherSuit
  deriving Eq


data Aspect = Blaze | Heaven | Duality | Shroom | Blood | Mirage | Mirror | Alchemy | Crown | Morph | Strange | OtherAspect
  deriving Eq
