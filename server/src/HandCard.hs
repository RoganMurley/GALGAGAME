{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module HandCard where

import Card (Card(..))
import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)
import Mirror (Mirror(..))


data HandCard = HandCard Card | KnownHandCard Card
  deriving (Eq, Generic, NFData, Show)


instance Mirror HandCard where
  mirror = id


instance ToJSON HandCard where
  toJSON (HandCard c) =
    object
    [ "known" .= False
    , "card"  .= c
    ]
  toJSON (KnownHandCard c) =
    object
    [ "known" .= True
    , "card"  .= c
    ]


anyCard :: HandCard -> Card
anyCard (HandCard card) = card
anyCard (KnownHandCard card) = card


knownCard :: HandCard -> Maybe Card
knownCard (KnownHandCard card) = Just card
knownCard _ = Nothing


reveal :: HandCard -> HandCard
reveal (HandCard c) = KnownHandCard c
reveal (KnownHandCard c) = KnownHandCard c


hide :: HandCard -> HandCard
hide (HandCard c) = HandCard c
hide (KnownHandCard c) = HandCard c
