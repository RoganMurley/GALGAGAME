module Player where

import Data.Aeson (ToJSON(..))

import Mirror (Mirror, mirror)


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)


instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"


other :: WhichPlayer -> WhichPlayer
other PlayerA = PlayerB
other PlayerB = PlayerA


perspective :: Mirror a => WhichPlayer -> (a -> a)
perspective PlayerA = id
perspective PlayerB = mirror
