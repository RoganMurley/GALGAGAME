{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Discard where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)

data CardDiscard = NoDiscard Int | CardDiscard
  deriving (Eq, Generic, NFData, Show)

instance ToJSON CardDiscard where
  toJSON (NoDiscard finalIndex) =
    object [
      "finalIndex" .= finalIndex
    ]
  toJSON CardDiscard = "discard"


isDiscard :: CardDiscard -> Bool
isDiscard CardDiscard = True
isDiscard _           = False
