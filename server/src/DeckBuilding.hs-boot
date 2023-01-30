module DeckBuilding where

import Card (Aspect)
import Data.Text (Text)

data Rune

instance Ord Rune

instance Eq Rune

instance Show Rune

getRuneName :: Rune -> Text
getRuneAspect :: Rune -> Aspect
getRuneByName :: Text -> Maybe Rune
