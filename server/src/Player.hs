{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Player where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Generic, NFData, Show)


instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"


other :: WhichPlayer -> WhichPlayer
other PlayerA = PlayerB
other PlayerB = PlayerA
