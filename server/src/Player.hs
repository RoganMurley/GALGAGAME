module Player where

import Data.Aeson (ToJSON(..))

import Mirror (Mirror(..))


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)


instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"


instance Mirror WhichPlayer where
  mirror PlayerA = PlayerB
  mirror PlayerB = PlayerA


other :: WhichPlayer -> WhichPlayer
other = mirror
