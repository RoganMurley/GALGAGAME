module DeckBuilding where

import Data.Text (Text)

data Rune

instance Ord Rune

instance Eq Rune

instance Show Rune

getRuneName :: Rune -> Text
getRuneByName :: Text -> Maybe Rune
