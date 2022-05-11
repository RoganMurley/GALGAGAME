{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bounce where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import GHC.Generics (Generic)
import Wheel (Wheel)

data CardBounce = BounceDiscard | BounceIndex Int Int
  deriving (Eq, Generic, NFData, Show)

instance ToJSON CardBounce where
  toJSON BounceDiscard = "bounceDiscard"
  toJSON (BounceIndex stackIndex handIndex) =
    object
      [ "handIndex" .= handIndex,
        "stackIndex" .= stackIndex
      ]

data BounceState = BounceState
  { stackIndex :: Int,
    handAIndex :: Int,
    handBIndex :: Int,
    bounces :: Wheel (Maybe CardBounce)
  }
  deriving (Show)