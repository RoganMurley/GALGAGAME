module Bounce where

import Data.Aeson (ToJSON(..), (.=), object)

data CardBounce = NoBounce Int | BounceDiscard | BounceIndex Int Int
  deriving (Show, Eq)

instance ToJSON CardBounce where
  toJSON (NoBounce finalStackIndex) =
    object [
      "finalStackIndex" .= finalStackIndex
    ]
  toJSON BounceDiscard = "bounceDiscard"
  toJSON (BounceIndex stackIndex handIndex) =
    object [
      "stackIndex" .= stackIndex,
      "handIndex" .= handIndex
    ]
