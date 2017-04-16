module Characters where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Cards
import Model
import Safe (headMay)


-- TYPES

type CharacterCards = (Card, Card, Card, Card)

data SelectedCharacters
    = NoneSelected
    | OneSelected   Character
    | TwoSelected   Character Character
    | ThreeSelected Character Character Character
    deriving (Eq, Show)

instance ToJSON SelectedCharacters where
  toJSON s = toJSON . toList $ s


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
    charmodel_pa         :: SelectedCharacters
  , charmodel_pb         :: SelectedCharacters
  , charmodel_characters :: [Character]
  } deriving (Eq, Show)

instance ToJSON CharModel where
  toJSON (CharModel selected _ characters) =
    object [
      "selecting" .= characters
    , "selected"  .= selected
    ]


type FinalSelection =
  (Character, Character, Character)


characterModelReverso :: CharModel -> CharModel
characterModelReverso (CharModel pa pb cs) =
  CharModel pb pa cs


initCharModel :: CharModel
initCharModel = CharModel NoneSelected NoneSelected allCharacters


allCharacters :: [Character]
allCharacters = [
    fire
  , thunder
  , calm
  , tempest
  , vortex
  , dust
  , frost
  ]


selectChar :: CharModel -> WhichPlayer -> Text -> CharModel
selectChar model@(CharModel { charmodel_pa = m }) PlayerA name =
  model { charmodel_pa = selectIndChar name m }
selectChar model@(CharModel { charmodel_pb = m }) PlayerB name =
  model { charmodel_pb = selectIndChar name m }


selectIndChar :: Text -> SelectedCharacters -> SelectedCharacters
selectIndChar name selected =
  if existingSelected
    then (
      case character of
        Just char ->
          case selected of
            NoneSelected ->
              OneSelected char
            OneSelected a ->
              TwoSelected a char
            TwoSelected a b ->
              ThreeSelected a b char
            ThreeSelected a b c  ->
              ThreeSelected a b c
        Nothing ->
          selected
      )
    else selected
  where
    nameMatch :: Character -> Bool
    nameMatch (Character n _) = n == name
    character :: Maybe Character
    character = headMay . (filter nameMatch) $ allCharacters
    existingSelected :: Bool
    existingSelected = not . (any nameMatch) . toList $ selected


toList :: SelectedCharacters -> [Character]
toList NoneSelected          = []
toList (OneSelected a)       = [ a ]
toList (TwoSelected a b)     = [ a, b ]
toList (ThreeSelected a b c) = [ a, b, c ]


-- CHARACTERS

cardPlaceholder :: Card
cardPlaceholder = Card "Placeholder" "For testing!" "goat.svg" "resolve.sfx" (\_ _ m -> m)

calm :: Character
calm = Character "The Calm" (cardBoomerang, cardCrossbow, cardPotion, cardReflect)

fire :: Character
fire = Character "The Fire" (cardDagger, cardFirestorm, cardOffering, cardConfound)

thunder :: Character
thunder = Character "The Thunder" (cardHammer, cardLightning, cardEcho, cardHubris)

tempest :: Character
tempest = Character "The Tempest" (cardVampire, cardSuccubus, cardSiren, cardReversal)

vortex :: Character
vortex = Character "The Vortex" (cardStaff, cardEnvy, cardMindhack, cardProphecy)

dust :: Character
dust = Character "The Dust" (cardShuriken, cardSuperego, cardMindgate, cardFeint)

frost :: Character
frost = Character "The Frost" (cardAxe, cardFrostbite, cardCrystal, cardFairy)
