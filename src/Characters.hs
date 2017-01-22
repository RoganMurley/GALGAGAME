module Characters where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Cards
import Model


-- TYPES

type CharacterCards = (Card, Card, Card, Card)
type SelectedCharacters = (Character, Character, Character)


data Character = Character
  { character_name  :: Text
  , character_cards :: CharacterCards
  } deriving (Eq, Show)

instance ToJSON Character where
  toJSON (Character name cards) =
      object [
        "name"  .= name
      , "cards" .= cards
      ]


data CharModel =
  CharModel {
    charmodel_pa         :: Maybe SelectedCharacters
  , charmodel_pb         :: Maybe SelectedCharacters
  , charmodel_characters :: [Character]
  } deriving (Eq, Show)

instance ToJSON CharModel where
  toJSON (CharModel _ _ characters) =
    object [
      "selecting" .= characters
    ]


characterModelReverso :: CharModel -> CharModel
characterModelReverso (CharModel pa pb cs) =
  CharModel pb pa cs


initCharModel :: CharModel
initCharModel = CharModel Nothing Nothing allCharacters


allCharacters :: [Character]
allCharacters = [
    protector
  , apollo
  , nemesis
  , drinker
  , oracle
  ]


selectChar :: CharModel -> WhichPlayer -> (Text, Text, Text) -> CharModel
selectChar model PlayerA characters =
  model { charmodel_pa = Just . textToCharacters $ characters }
selectChar model PlayerB characters =
  model { charmodel_pb = Just . textToCharacters $ characters }


textToCharacters :: (Text, Text, Text) -> SelectedCharacters
textToCharacters (a, b, c) = (f a, f b, f c)
  where
    f :: Text -> Character
    f t = head . (filter (\(Character n _) -> n == t )) $ allCharacters


-- CHARACTERS

cardPlaceholder :: Card
cardPlaceholder = Card "Placeholder" "For testing!" "goat.svg" (\_ _ m -> m)

protector :: Character
protector = Character "Protector" (cardBoomerang, cardAgility, cardPotion, cardReflect)

apollo :: Character
apollo = Character "Apollo" (cardDagger, cardFireball, cardOffering, cardConfound)

nemesis :: Character
nemesis = Character "Nemesis" (cardHammer, cardPlaceholder, cardSickness, cardHubris)

drinker :: Character
drinker = Character "Drinker" (cardVampire, cardSuccubus, cardSiren, cardReversal)

oracle :: Character
oracle = Character "Oracle" (cardObscurer, cardPlaceholder, cardZen, cardProphecy)
