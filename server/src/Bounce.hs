module Bounce where

import Data.Aeson (ToJSON(..), (.=), object)

data CardBounce = BounceDiscard | BounceIndex Int Int
  deriving (Show, Eq)

instance ToJSON CardBounce where
  toJSON BounceDiscard = "bounceDiscard"
  toJSON (BounceIndex stackIndex handIndex) =
    object [
      "handIndex"  .= handIndex
    , "stackIndex" .= stackIndex
    ]
