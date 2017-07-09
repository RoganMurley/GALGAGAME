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
  , character_cards :: CharacterCards
  } deriving (Eq, Show)

instance ToJSON Character where
  toJSON Character{ character_name, character_cards } =
      object [
        "name"    .= character_name
      , "img_url" .= (card_img . (\(c, _, _, _) -> c) $ character_cards :: Text)
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
    nameMatch Character{character_name} = character_name == name
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
    striker
  , breaker
  , bouncer
  , shielder
  , drinker
  , watcher
  , balancer
  , collector
  ]

striker :: Character
striker =
  Character
    "Striker"
    (Cards.dagger, Cards.fireball, Cards.offering, Cards.confound)

breaker :: Character
breaker =
  Character
    "Breaker"
    (Cards.hammer, Cards.lightning, Cards.exile, Cards.hubris)

drinker :: Character
drinker =
  Character
    "Drinker"
    (Cards.scythe, Cards.bloodsucker, Cards.serpent, Cards.reversal)

watcher :: Character
watcher =
  Character
    "Watcher"
    (Cards.staff, Cards.overburden, Cards.mindhack, Cards.prophecy)

shielder :: Character
shielder =
  Character
    "Shielder"
    (Cards.sword, Cards.soulburn, Cards.potion, Cards.reflect)

bouncer :: Character
bouncer =
  Character
    "Bouncer"
    (Cards.boomerang, Cards.overwhelm, Cards.echo, Cards.feint)

balancer :: Character
balancer =
  Character
    "Balancer"
    (Cards.katana, Cards.curse, Cards.bless, Cards.balance)

collector :: Character
collector =
  Character
    "Collecter"
    (Cards.runeblade, Cards.surge, Cards.hoard, Cards.transmute)

-- augmenter :: Character
-- augmenter =
--   Character
--     "Augment"
--     (Cards.fist, Cards.supercharge, Cards.charge, Cards.echo)
