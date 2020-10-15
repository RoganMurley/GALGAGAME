module DeckBuilding where

import Card (Card(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Mirror (Mirror(..))
import Model (Deck)
import Player (WhichPlayer(..))

import qualified Cards
import Data.Set as Set


-- Rune
data Rune = Rune
  { rune_name  :: Text
  , rune_desc  :: Text
  , rune_img   :: Text
  , rune_cards :: RuneCards
  } deriving (Eq, Show)


instance ToJSON Rune where
  toJSON Rune{ rune_name, rune_desc, rune_img, rune_cards } =
    object
      [ "name"    .= rune_name
      , "desc"    .= rune_desc
      , "img_url" .= rune_img
      , "cards"   .= rune_cards
      ]


type RuneCards = (Card, Card, Card, Card)


-- Character
data Character = Character
  { character_name    :: Text
  , character_img_url :: Text
  , character_rune_a  :: Rune
  , character_rune_b  :: Rune
  , character_rune_c  :: Rune
  }
  deriving (Eq, Show)


instance ToJSON Character where
  toJSON (Character name imgUrl runeA runeB runeC) =
    object
      [ "name"    .= name
      , "img_url" .= imgUrl
      , "rune_a"  .= runeA
      , "rune_b"  .= runeB
      , "rune_c"  .= runeC
      ]


-- DeckBuilding
data DeckBuilding =
  DeckBuilding {
    deckbuilding_pa :: Maybe Character
  , deckbuilding_pb :: Maybe Character
  } deriving (Eq, Show)


instance ToJSON DeckBuilding where
  toJSON DeckBuilding{ deckbuilding_pa } =
    object
      [ "character"      .= deckbuilding_pa
      , "all_characters" .= allCharacters
      , "all_runes"      .= allRunes
      ]


instance Mirror DeckBuilding where
  mirror (DeckBuilding pa pb) = DeckBuilding pb pa


initDeckBuilding :: Maybe Character -> Maybe Character -> DeckBuilding
initDeckBuilding = DeckBuilding


selectCharacter :: DeckBuilding -> WhichPlayer -> Character -> DeckBuilding
selectCharacter deckModel which character =
  case which of
    PlayerA ->
      DeckBuilding {
        deckbuilding_pa = Just character
      , deckbuilding_pb = deckbuilding_pb deckModel
      }
    PlayerB ->
      DeckBuilding {
        deckbuilding_pa = deckbuilding_pa deckModel
      , deckbuilding_pb = Just character
      }


isReady :: DeckBuilding -> WhichPlayer -> Bool
isReady deckModel which =
  case which of
    PlayerA ->
      isJust $ deckbuilding_pa deckModel
    PlayerB ->
      isJust $ deckbuilding_pa deckModel


-- CharacterChoice
data CharacterChoice = CharacterChoice
  { choice_name :: Text
  , choice_ra   :: Text
  , choice_rb   :: Text
  , choice_rc   :: Text
  } deriving (Eq, Show)


instance FromJSON CharacterChoice where
  parseJSON =
    withObject "CharacterChoice" $
    \o ->
      CharacterChoice
        <$> o .: "character_name"
        <*> o .: "rune_a"
        <*> o .: "rune_b"
        <*> o .: "rune_c"


choiceToCharacter :: CharacterChoice -> Either Text Character
choiceToCharacter CharacterChoice{choice_name, choice_ra, choice_rb, choice_rc} =
  let
    baseCharacter :: Either Text Character
    baseCharacter = getCharacter choice_name
    uniqueChoices :: Bool
    uniqueChoices = Set.size (Set.fromList [choice_ra, choice_rb, choice_rc]) == 3
  in
  if uniqueChoices then
    Character
      <$> (character_name <$> baseCharacter)
      <*> (character_img_url <$> baseCharacter)
      <*> getRune choice_ra
      <*> getRune choice_rb
      <*> getRune choice_rc
  else
    Left "Rune choices were not unique"


getCharacter :: Text -> Either Text Character
getCharacter name =
  case find (\Character{character_name} -> character_name == name) allCharacters of
    Just character ->
      Right character
    Nothing ->
      Left "Invalid character name"


getRune :: Text -> Either Text Rune
getRune name =
  case find (\Rune{rune_name} -> rune_name == name) allRunes of
    Just rune ->
      Right rune
    Nothing ->
      Left "Invalid rune name"


-- RUNES
allRunes :: [Rune]
allRunes =
  [ lust
  , gluttony
  , greed
  , sloth
  , wrath
  , envy
  , pride
  , fungus
  , base
  --, duelist
  ]


wrath :: Rune
wrath =
  Rune
    "FIRE"
    "You're dangerous and unpredictable."
    "fire-circle.png"
    (Cards.fireSword, Cards.fireball, Cards.fireCup, Cards.confound)


pride :: Rune
pride =
  Rune
    "THUNDER"
    "You punish mistakes."
    "thunder-circle.png"
    (Cards.hammer, Cards.lightning, Cards.feint, Cards.hubris)


fungus :: Rune
fungus =
  Rune
    "FUNGUS"
    "You steal life."
    "fungus-seal.png"
    (Cards.scythe, Cards.bloodsucker, Cards.serpent, Cards.reversal)


lust :: Rune
lust =
  Rune
    "BLOOD"
    "You steal life."
    "blood-circle.png"
    (Cards.bloodSword, Cards.bloodHex, Cards.offering, Cards.sacrifice)


gluttony :: Rune
gluttony =
  Rune
    "VISION"
    "You play mind games."
    "vision-circle.png"
    (Cards.staff, Cards.surge, Cards.mimic, Cards.prophecy)


envy :: Rune
envy =
  Rune
    "WATER"
    "You protect yourself."
    "water-circle.png"
    (Cards.grudge, Cards.overwhelm, Cards.potion, Cards.reflect)


sloth :: Rune
sloth =
  Rune
    "DUALITY"
    "You're strong but hard to control."
    "duality-coin.png"
    (Cards.katana, Cards.curse, Cards.bless, Cards.balance)


greed :: Rune
greed =
  Rune
    "GOLD"
    "You punish hoarding."
    "gold-circle.png"
    (Cards.relicblade, Cards.greed, Cards.echo, Cards.alchemy)


duelist :: Rune
duelist =
  Rune
    "Crown"
    "You use every advantage you have."
    "subjugate.png"
    (Cards.lance, Cards.meltdown, Cards.taunt, Cards.subjugate)


base :: Rune
base =
  Rune
    "BEGINNINGS"
    "You are basic."
    "basic-coin.png"
    (Cards.basicSword, Cards.basicWand, Cards.basicCup, Cards.basicCoin)


-- Characters
allCharacters :: [Character]
allCharacters = [
    catherine
  , marcus
  , freja
  ]


catherine :: Character
catherine =
  Character
    "Catherine"
    "/img/textures/confound.png"
    wrath
    fungus
    envy


marcus :: Character
marcus =
  Character
    "Marcus"
    "/img/textures/hubris.png"
    pride
    gluttony
    sloth


freja :: Character
freja =
  Character
    "Freja"
    "/img/textures/alchemy.png"
    greed
    envy
    wrath


characterCards :: Character -> Deck
characterCards Character{ character_rune_a, character_rune_b, character_rune_c } =
  concat $ (\(p, q, r, s) -> [p, q, r, s]) <$>
    [rune_cards character_rune_a, rune_cards character_rune_b, rune_cards character_rune_c]
