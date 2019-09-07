module Limbo where

import Data.Aeson (ToJSON(..), (.=), object)

data CardLimbo = NoLimbo Int | CardLimbo
  deriving (Show, Eq)

instance ToJSON CardLimbo where
  toJSON (NoLimbo finalStackIndex) =
    object [
      "finalStackIndex" .= finalStackIndex
    ]
  toJSON CardLimbo = "limbo"
