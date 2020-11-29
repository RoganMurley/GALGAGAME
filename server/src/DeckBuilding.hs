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
  [
    blazeRune
  , heavenRune
  , shroomRune
  -- , bloodRune
  , mirageRune
  , mirrorRune
  , dualityRune
  , alchemyRune
  --, crownRune
  , morphRune
  ]


blazeRune :: Rune
blazeRune =
  Rune
    "BLAZE"
    "Blazing heart."
    "cards/blaze/coin.png"
    (Cards.blazeSword, Cards.blazeWand, Cards.blazeCup, Cards.blazeCoin)


heavenRune :: Rune
heavenRune =
  Rune
    "HEAVEN"
    "It appears the heavens have opened."
    "cards/heaven/coin.png"
    (Cards.heavenSword, Cards.heavenWand, Cards.heavenCup, Cards.heavenCoin)


shroomRune :: Rune
shroomRune =
  Rune
    "SHROOM"
    "Earth to earth."
    "cards/shroom/coin.png"
    (Cards.shroomSword, Cards.shroomWand, Cards.shroomCup, Cards.shroomCoin)


bloodRune :: Rune
bloodRune =
  Rune
    "BLOOD"
    "You steal life."
    "cards/blood/coin.png"
    (Cards.bloodSword, Cards.bloodWand, Cards.bloodCup, Cards.bloodCoin)


mirageRune :: Rune
mirageRune =
  Rune
    "MIRAGE"
    "Seeing is believing."
    "cards/mirage/coin.png"
    (Cards.mirageSword, Cards.mirageWand, Cards.mirageCup, Cards.mirageCoin)


mirrorRune :: Rune
mirrorRune =
  Rune
    "MIRROR"
    "A reflection of a reflection."
    "cards/mirror/coin.png"
    (Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorCup, Cards.mirrorCoin)


dualityRune :: Rune
dualityRune =
  Rune
    "DUALITY"
    "Strength from weakness, weakness from strength."
    "cards/duality/coin.png"
    (Cards.dualitySword, Cards.dualityWand, Cards.dualityCup, Cards.dualityCoin)


alchemyRune :: Rune
alchemyRune =
  Rune
    "ALCHEMY"
    "For the lead hearted."
    "cards/alchemy/coin.png"
    (Cards.alchemySword, Cards.alchemyWand, Cards.alchemyCup, Cards.alchemyCoin)


crownRune :: Rune
crownRune =
  Rune
    "CROWN"
    "You use every advantage you have."
    "cards/crown/coin.png"
    (Cards.crownSword, Cards.crownWand, Cards.crownCup, Cards.crownCoin)


morphRune :: Rune
morphRune =
  Rune
    "MORPH"
    "For the mutable mind."
    "cards/morph/coin.png"
    (Cards.morphSword, Cards.morphWand, Cards.morphCup, Cards.morphCoin)


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
    "0 / The Fool"
    "/img/textures/confound.png"
    blazeRune
    shroomRune
    mirrorRune


marcus :: Character
marcus =
  Character
    "I / The Magician"
    "/img/textures/hubris.png"
    alchemyRune
    mirrorRune
    morphRune


freja :: Character
freja =
  Character
    "II / The High Priestess"
    "/img/textures/alchemy.png"
    heavenRune
    mirageRune
    dualityRune


characterCards :: Character -> Deck
characterCards Character{ character_rune_a, character_rune_b, character_rune_c } =
  concat $ (\(p, q, r, s) -> [p, q, r, s]) <$>
    [rune_cards character_rune_a, rune_cards character_rune_b, rune_cards character_rune_c]
