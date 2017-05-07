module Characters where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Safe (headMay)

import qualified Cards

import Model (Card(..))
import Player (WhichPlayer(..))


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
  , character_img :: Text
  , character_cards :: CharacterCards
  } deriving (Eq, Show)

instance ToJSON Character where
  toJSON Character{ character_name, character_img, character_cards } =
      object [
        "name"    .= character_name
      , "img_url" .= character_img
      , "cards"   .= character_cards
      ]


data CharModel =
  CharModel {
    charmodel_pa         :: SelectedCharacters
  , charmodel_pb         :: SelectedCharacters
  , charmodel_characters :: [Character]
  } deriving (Eq, Show)

instance ToJSON CharModel where
  toJSON CharModel{ charmodel_pa, charmodel_characters } =
    object [
      "selecting" .= charmodel_pa
    , "selected"  .= charmodel_characters
    ]


type FinalSelection = (Character, Character, Character)


characterModelReverso :: CharModel -> CharModel
characterModelReverso (CharModel pa pb cs) =
  CharModel pb pa cs


initCharModel :: CharModel
initCharModel = CharModel NoneSelected NoneSelected allCharacters


selectChar :: CharModel -> WhichPlayer -> Text -> CharModel
selectChar model@(CharModel { charmodel_pa = m }) PlayerA name =
  model { charmodel_pa = selectIndChar name m }
selectChar model@(CharModel { charmodel_pb = m }) PlayerB name =
  model { charmodel_pb = selectIndChar name m }


selectIndChar :: Text -> SelectedCharacters -> SelectedCharacters
selectIndChar name selected
  | existingSelected =
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
  | otherwise =
    selected
  where
    nameMatch :: Character -> Bool
    nameMatch (Character n _ _) = n == name
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
allCharacters :: [Character]
allCharacters = [
    flame
  , thunder
  , frost
  , tempest
  , mist
  , vortex
  , calm
  ]

flame :: Character
flame =
  Character
    "Flame"
    "dragon/dragon.svg"
    (Cards.dragon, Cards.firestorm, Cards.offering, Cards.haze)

frost :: Character
frost =
  Character
    "Frost"
    "gem/gem.svg"
    (Cards.gem, Cards.blizzard, Cards.crystal, Cards.alchemy)

thunder :: Character
thunder =
  Character
    "Thunder"
    "stag/stag.svg"
    (Cards.stag, Cards.lightning, Cards.echo, Cards.hubris)

tempest :: Character
tempest =
  Character
    "Tempest"
    "octopus/octopus.svg"
    (Cards.octopus, Cards.tentacles, Cards.siren, Cards.reversal)

vortex :: Character
vortex =
  Character
    "Vortex"
    "owl/owl.svg"
    (Cards.owl, Cards.twister, Cards.hypnosis, Cards.prophecy)

mist :: Character
mist =
  Character
    "Mist"
    "monkey/monkey.svg"
    (Cards.monkey, Cards.monsoon, Cards.mindgate, Cards.feint)

calm :: Character
calm =
  Character
    "Calm"
    "turtle/turtle.svg"
    (Cards.turtle, Cards.gust, Cards.soup, Cards.reflect)
