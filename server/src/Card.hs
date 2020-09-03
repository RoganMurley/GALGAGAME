module Card where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import Player (WhichPlayer(..))

import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta


instance Eq Card where
  (Card n1 d1 i1 c1 _) == (Card n2 d2 i2 c2 _) =
    n1 == n2 && d1 == d2 && i1 == i2 && c1 == c2


instance Show Card where
  show = cs . card_name


instance ToJSON Card where
  toJSON (Card name desc imageURL col _) =
    object
      [ "name"     .= toUpper name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      , "col"      .= col
      ]


data Card = Card
  { card_name :: Text
  , card_desc :: Text
  , card_img  :: Text
  , card_col  :: CardCol
  , card_eff  :: WhichPlayer -> Beta.Program ()
  }



description :: Card -> Text
description Card{ card_name, card_desc } =
  "(" <> card_name <> ": " <> card_desc <> ")"


data CardCol = Red | Orange | Yellow | Green | Blue | White | Violet | Copper | Mystery
  deriving Eq


instance ToJSON CardCol where
  toJSON Red     = "red"
  toJSON Orange  = "orange"
  toJSON Yellow  = "yellow"
  toJSON Green   = "green"
  toJSON Blue    = "blue"
  toJSON White   = "white"
  toJSON Violet  = "violet"
  toJSON Copper  = "copper"
  toJSON Mystery = "mystery"
