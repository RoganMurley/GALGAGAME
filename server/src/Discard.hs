{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Discard where

import Card (Card)
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import HandCard (HandCard (..))

data CardDiscard = NoDiscard (Maybe Card) Int | CardDiscard HandCard
  deriving (Eq, Generic, NFData, Show)

instance ToJSON CardDiscard where
  toJSON (NoDiscard card finalIndex) =
    object
      [ "discard" .= ("NoDiscard" :: Text),
        "card" .= card,
        "finalIndex" .= finalIndex
      ]
  toJSON (CardDiscard card) =
    object
      [ "discard" .= ("CardDiscard" :: Text),
        "card" .= card
      ]

isDiscard :: CardDiscard -> Bool
isDiscard (CardDiscard _) = True
isDiscard _ = False
