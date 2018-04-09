module StackCard where

import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)


data StackCard = StackCard
  { stackcard_owner :: WhichPlayer
  , stackcard_card  :: Card
  }
  deriving (Eq, Show)


instance ToJSON StackCard where
  toJSON StackCard{ stackcard_owner, stackcard_card } =
    object
      [ "owner" .= stackcard_owner
      , "card"  .= stackcard_card
      ]


instance Mirror StackCard where
  mirror (StackCard p c) = StackCard (other p) c


changeOwner :: StackCard -> StackCard
changeOwner = mirror


isOwner :: WhichPlayer -> StackCard -> Bool
isOwner w (StackCard o _) = w == o
