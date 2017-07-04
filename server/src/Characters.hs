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
  , character_img   :: Text
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
      "selecting" .= charmodel_characters
    , "selected"  .= charmodel_pa
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
    ranger
  , striker
  , breaker
  , drinker
  , trickster
  , watcher
  , seeker
  ]

striker :: Character
striker =
  Character
    "Striker"
    "striker/fireball.svg"
    (Cards.dagger, Cards.fireball, Cards.offering, Cards.confound)

breaker :: Character
breaker =
  Character
    "Breaker"
    "breaker/lightning.svg"
    (Cards.hammer, Cards.lightning, Cards.decree, Cards.hubris)

drinker :: Character
drinker =
  Character
    "Drinker"
    "drinker/bloodsucker.svg"
    (Cards.scythe, Cards.bloodsucker, Cards.serpent, Cards.reversal)

seeker :: Character
seeker =
  Character
    "Seeker"
    "seeker/curse.svg"
    (Cards.axe, Cards.curse, Cards.bless, Cards.alchemy)

watcher :: Character
watcher =
  Character
    "Watcher"
    "watcher/prophecy.svg"
    (Cards.staff, Cards.greed, Cards.mindhack, Cards.prophecy)

ranger :: Character
ranger =
  Character
    "Ranger"
    "ranger/reflect.svg"
    (Cards.boomerang, Cards.crossbow, Cards.potion, Cards.reflect)

trickster :: Character
trickster =
  Character
    "Trickster"
    "trickster/overwhelm.svg"
    (Cards.sword, Cards.overwhelm, Cards.echo, Cards.feint)
