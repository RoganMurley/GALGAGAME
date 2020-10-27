module Transmutation where

import Data.Aeson (ToJSON(..), Value(Null), (.=), object)
import Mirror (Mirror(..))
import StackCard (StackCard)


data Transmutation = Transmutation StackCard StackCard | NoTransmutation
  deriving (Show, Eq)


instance ToJSON Transmutation where
  toJSON (Transmutation ca cb) =
    object
    [ "cardA"     .= ca
    , "cardB"     .= cb
    ]
  toJSON NoTransmutation = Null


instance Mirror Transmutation where
  mirror (Transmutation ca cb) = Transmutation (mirror ca) (mirror cb)
  mirror NoTransmutation       = NoTransmutation
