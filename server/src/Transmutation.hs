module Transmutation where

import Data.Aeson (ToJSON(..), Value(Null), (.=), object)
import Mirror (Mirror(..))
import StackCard (StackCard)


data Transmutation = Transmutation StackCard StackCard | NoTransmutation
  deriving (Show, Eq)


instance ToJSON Transmutation where
  toJSON (Transmutation ca cb) =
    object
    [ "cardA" .= ca
    , "cardB" .= cb
    ]
  toJSON NoTransmutation = Null


instance Mirror Transmutation where
  mirror (Transmutation ca cb) = Transmutation (mirror ca) (mirror cb)
  mirror NoTransmutation       = NoTransmutation


headTransmuter :: (StackCard -> StackCard) -> (Int -> StackCard -> Maybe Transmutation)
headTransmuter f = f'
  where
    f' :: Int -> StackCard -> Maybe Transmutation
    f' 1 sc = Just $ Transmutation sc (f sc)
    f' _ _  = Nothing
