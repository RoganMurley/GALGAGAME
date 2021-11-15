{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bounce where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)

data CardBounce = BounceDiscard | BounceIndex Int Int
  deriving (Eq, Generic, NFData, Show)

instance ToJSON CardBounce where
  toJSON BounceDiscard = "bounceDiscard"
  toJSON (BounceIndex stackIndex handIndex) =
    object [
      "handIndex"  .= handIndex
    , "stackIndex" .= stackIndex
    ]
