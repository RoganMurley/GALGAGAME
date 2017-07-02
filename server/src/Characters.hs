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
    fire
  , thunder
  , seek
  , feast
  , trick
  , future
  , shield
  ]

fire :: Character
fire =
  Character
    "Fire"
    "fire/fireball.svg"
    (Cards.dagger, Cards.fireball, Cards.offering, Cards.confound)

seek :: Character
seek =
  Character
    "Seek"
    "seek/curse.svg"
    (Cards.axe, Cards.curse, Cards.bless, Cards.alchemy)

thunder :: Character
thunder =
  Character
    "Thunder"
    "thunder/lightning.svg"
    (Cards.hammer, Cards.lightning, Cards.echo, Cards.hubris)

feast :: Character
feast =
  Character
    "Feast"
    "feast/bloodsucker.svg"
    (Cards.scythe, Cards.bloodsucker, Cards.theBook, Cards.reversal)

future :: Character
future =
  Character
    "Future"
    "future/prophecy.svg"
    (Cards.staff, Cards.greed, Cards.mindhack, Cards.prophecy)

trick :: Character
trick =
  Character
    "Trick"
    "trick/superego.svg"
    (Cards.shuriken, Cards.superego, Cards.mindgate, Cards.feint)

shield :: Character
shield =
  Character
    "Shield"
    "shield/reflect.svg"
    (Cards.crossbow, Cards.boomerang, Cards.potion, Cards.reflect)
