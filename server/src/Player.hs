module Player where

import Data.Aeson (ToJSON(..))


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)


instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"


other :: WhichPlayer -> WhichPlayer
other PlayerA = PlayerB
other PlayerB = PlayerA
