{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module StackCard where

import Card (Card(..))
import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)


data StackCard = StackCard
  { stackcard_owner :: WhichPlayer
  , stackcard_card  :: Card
  }
  deriving (Eq, Generic, NFData, Show)


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


cardMap :: (Card -> Card) -> StackCard -> StackCard
cardMap f (StackCard { stackcard_owner, stackcard_card }) =
  StackCard {
    stackcard_owner = stackcard_owner,
    stackcard_card = f stackcard_card
  }
