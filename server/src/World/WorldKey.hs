{-# LANGUAGE DeriveGeneric #-}
module World.WorldKey where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)


data WorldKey
  = Start
  | Crown
  | Understanding
  | Wisdom
  | Severity
  | Mercy
  | Splendor
  | Victory
  | Beauty
  | Foundation
  | Kingdom
  deriving (Eq, Generic, Ord, Show)


instance ToJSON WorldKey

instance FromJSON WorldKey
