module DeckBuilding where

import Card (Card(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Life (Life, initMaxLife)
import Mirror (Mirror(..))
import Model (Deck)
import Player (WhichPlayer(..))

import qualified Cards
import Data.Set as Set


-- Rune
data Rune = Rune
  { rune_name  :: Text
  , rune_img   :: Text
  , rune_cards :: RuneCards
  } deriving (Eq, Show)


instance ToJSON Rune where
  toJSON Rune{ rune_name, rune_img, rune_cards } =
    object
      [ "name"    .= rune_name
      , "img_url" .= rune_img
      , "cards"   .= rune_cards
      ]


type RuneCards = (Card, Card, Card, Card)


-- Character
data Character = Character
  { character_name    :: Text
  , character_img_url :: Text
  , character_choice  :: Either (Rune, Rune, Rune) Deck
  , character_maxLife :: Life
  }
  deriving (Eq, Show)


instance ToJSON Character where
  toJSON (Character name imgUrl (Left (runeA, runeB, runeC)) _) =
    object
      [ "name"    .= name
      , "img_url" .= imgUrl
      , "rune_a"  .= runeA
      , "rune_b"  .= runeB
      , "rune_c"  .= runeC
      ]
  toJSON (Character name imgUrl _ _) =
    object
      [ "name"    .= name
      , "img_url" .= imgUrl
      ]


-- DeckBuilding
data DeckBuilding =
  DeckBuilding
  { deckbuilding_pa :: Maybe Character
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
      isJust $ deckbuilding_pb deckModel


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
    makeCharacter :: Text -> Text -> Rune -> Rune -> Rune -> Character
    makeCharacter name imgUrl choicePa choicePb choicePc =
      Character name imgUrl (Left (choicePa, choicePb, choicePc)) initMaxLife
  in
  if uniqueChoices then
    makeCharacter
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
  [ blazeRune
  , tideRune
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


tideRune :: Rune
tideRune =
  Rune
    "TIDE"
    "cards/tide/coin.png"
    (Cards.tideSword, Cards.tideWand, Cards.tideGrail, Cards.tideCoin)


blazeRune :: Rune
blazeRune =
  Rune
    "BLAZE"
    "cards/blaze/coin.png"
    (Cards.blazeSword, Cards.blazeWand, Cards.blazeGrail, Cards.blazeCoin)


heavenRune :: Rune
heavenRune =
  Rune
    "HEAVEN"
    "cards/heaven/coin.png"
    (Cards.heavenSword, Cards.heavenWand, Cards.heavenGrail, Cards.heavenCoin)


shroomRune :: Rune
shroomRune =
  Rune
    "SHROOM"
    "cards/shroom/coin.png"
    (Cards.shroomSword, Cards.shroomWand, Cards.shroomGrail, Cards.shroomCoin)


bloodRune :: Rune
bloodRune =
  Rune
    "BLOOD"
    "cards/blood/coin.png"
    (Cards.bloodSword, Cards.bloodWand, Cards.bloodGrail, Cards.bloodCoin)


mirageRune :: Rune
mirageRune =
  Rune
    "MIRAGE"
    "cards/mirage/coin.png"
    (Cards.mirageSword, Cards.mirageWand, Cards.mirageGrail, Cards.mirageCoin)


mirrorRune :: Rune
mirrorRune =
  Rune
    "MIRROR"
    "cards/mirror/coin.png"
    (Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorGrail, Cards.mirrorCoin)


dualityRune :: Rune
dualityRune =
  Rune
    "DUALITY"
    "cards/duality/coin.png"
    (Cards.dualitySword, Cards.dualityWand, Cards.dualityGrail, Cards.dualityCoin)


alchemyRune :: Rune
alchemyRune =
  Rune
    "ALCHEMY"
    "cards/alchemy/coin.png"
    (Cards.alchemySword, Cards.alchemyWand, Cards.alchemyGrail, Cards.alchemyCoin)


crownRune :: Rune
crownRune =
  Rune
    "CROWN"
    "cards/crown/coin.png"
    (Cards.crownSword, Cards.crownWand, Cards.crownGrail, Cards.crownCoin)


morphRune :: Rune
morphRune =
  Rune
    "MORPH"
    "cards/morph/coin.png"
    (Cards.morphSword, Cards.morphWand, Cards.morphGrail, Cards.morphCoin)


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
    (Left (blazeRune, shroomRune, mirrorRune))
    initMaxLife


marcus :: Character
marcus =
  Character
    "I / The Magician"
    "/img/textures/hubris.png"
    (Left (alchemyRune, mirrorRune, morphRune))
    initMaxLife


freja :: Character
freja =
  Character
    "II / The High Priestess"
    "/img/textures/alchemy.png"
    (Left (heavenRune, mirageRune, dualityRune))
    initMaxLife


characterCards :: Character -> Deck
characterCards Character{ character_choice } =
  case character_choice of
    Left (runeA, runeB, runeC) ->
      concat $ (\(p, q, r, s) -> [p, q, r, s]) <$>
        [rune_cards runeA, rune_cards runeB, rune_cards runeC]
    Right deck ->
      deck
