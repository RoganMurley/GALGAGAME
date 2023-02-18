{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Transmutation where

import Card (Card (..), Status (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (nubBy)
import GHC.Generics (Generic)
import Mirror (Mirror (..))
import StackCard (StackCard (..))

data Transmutation = Transmutation StackCard StackCard
  deriving (Eq, Generic, NFData, Show)

instance ToJSON Transmutation where
  toJSON (Transmutation ca cb) =
    object
      [ "cardA" .= ca,
        "cardB" .= cb
      ]

instance Mirror Transmutation where
  mirror (Transmutation ca cb) = Transmutation (mirror ca) (mirror cb)

transmuteToCard :: Card -> StackCard -> StackCard
transmuteToCard targetCard stackCard =
  StackCard
    { stackcard_owner = stackcard_owner stackCard,
      stackcard_card =
        targetCard
          { card_statuses =
              mergeStatuses
                (card_statuses . stackcard_card $ stackCard)
                (card_statuses targetCard)
          }
    }

mergeStatuses :: [Status] -> [Status] -> [Status]
mergeStatuses xs ys =
  -- Remove duplicate fragile statuses
  nubBy
    (\x y -> x == y && x == StatusFragile)
    $ xs ++ ys

removeTransmuteToSelf :: Maybe Transmutation -> Maybe Transmutation
removeTransmuteToSelf (Just (Transmutation ca cb)) =
  if ca == cb
    then Nothing
    else Just (Transmutation ca cb)
removeTransmuteToSelf Nothing = Nothing
