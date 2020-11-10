module Bounce where

import Data.Aeson (ToJSON(..), (.=), object)

data CardBounce = BounceDiscard | BounceIndex Int
  deriving (Show, Eq)

instance ToJSON CardBounce where
  toJSON BounceDiscard = "bounceDiscard"
  toJSON (BounceIndex handIndex) =
    object [
      "handIndex" .= handIndex
    ]
