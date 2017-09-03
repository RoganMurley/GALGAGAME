module Username where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)


newtype Username = Username Text
  deriving (Eq, Show)


instance ToJSON Username where
  toJSON (Username name) = toJSON name
