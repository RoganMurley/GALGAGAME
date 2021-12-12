{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Transmutation where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)
import Mirror (Mirror(..))
import Card(Card(..))
import StackCard (StackCard(..))


data Transmutation = Transmutation StackCard StackCard
  deriving (Eq, Generic, NFData, Show)


instance ToJSON Transmutation where
  toJSON (Transmutation ca cb) =
    object
    [ "cardA" .= ca
    , "cardB" .= cb
    ]


instance Mirror Transmutation where
  mirror (Transmutation ca cb) = Transmutation (mirror ca) (mirror cb)


transmuteToCard :: Card -> StackCard -> StackCard
transmuteToCard targetCard stackCard =
  StackCard
    { stackcard_owner = stackcard_owner stackCard
    , stackcard_card = targetCard { card_statuses = card_statuses . stackcard_card $ stackCard }
    }
