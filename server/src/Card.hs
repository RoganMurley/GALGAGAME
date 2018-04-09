module Card where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import Player (WhichPlayer(..))

import {-# SOURCE #-} qualified DSL.Beta.DSL as Beta


instance Eq Card where
  (Card n1 d1 i1 _) == (Card n2 d2 i2 _) =
    n1 == n2 && d1 == d2 && i1 == i2


instance Show Card where
  show = cs . card_name


instance ToJSON Card where
  toJSON (Card name desc imageURL _) =
    object
      [ "name"     .= name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      ]


data Card = Card
  { card_name :: Text
  , card_desc :: Text
  , card_img  :: Text
  , card_eff  :: WhichPlayer -> Beta.Program ()
  }



description :: Card -> Text
description Card{ card_name, card_desc } =
  "(" <> toUpper card_name <> ": " <> card_desc <> ")"
