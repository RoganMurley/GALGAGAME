module DeckBuilding where

import Card (Card(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject, withText)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Mirror (Mirror(..))
import Model (Deck)
import Player (WhichPlayer(..))

import qualified Cards


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


instance FromJSON Rune where
  parseJSON =
    withText "Rune" $
    \v ->
      case find (\Rune{rune_name} -> rune_name == v) allRunes of
        Just rune ->
          return rune
        Nothing ->
          fail "Invalid rune"



type RuneCards = (Card, Card, Card, Card)


-- Character
data Character = Character
  { character_name   :: Text
  , character_rune_a :: Rune
  , character_rune_b :: Rune
  , character_rune_c :: Rune
  }
  deriving (Eq, Show)


instance ToJSON Character where
  toJSON (Character name runeA runeB runeC) =
    object
      [ "name"   .= name
      , "rune_a" .= runeA
      , "rune_b" .= runeB
      , "rune_c" .= runeC
      ]


instance FromJSON Character where
  parseJSON =
    withObject "Character" $
      \v -> Character
        <$> v .: "name"
        <*> v .: "rune_a"
        <*> v .: "rune_b"
        <*> v .: "rune_c"


-- DeckBuilding
data DeckBuilding =
  DeckBuilding {
    deckbuilding_pa         :: Maybe Character
  , deckbuilding_pb         :: Maybe Character
  } deriving (Eq, Show)


instance ToJSON DeckBuilding where
  toJSON DeckBuilding{ deckbuilding_pa } =
    object
      [ "character"      .= deckbuilding_pa
      , "all_characters" .= allCharacters
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
  ]


wrath :: Rune
wrath =
  Rune
    "Wrath"
    "confound.png"
    (Cards.missile, Cards.fireball, Cards.offering, Cards.confound)


pride :: Rune
pride =
  Rune
    "Pride"
    "hubris.png"
    (Cards.hammer, Cards.lightning, Cards.feint, Cards.hubris)


lust :: Rune
lust =
  Rune
    "Lust"
    "reverse.png"
    (Cards.scythe, Cards.bloodsucker, Cards.serpent, Cards.reversal)


gluttony :: Rune
gluttony =
  Rune
    "Gluttony"
    "prophecy.png"
    (Cards.staff, Cards.surge, Cards.mimic, Cards.prophecy)


envy :: Rune
envy =
  Rune
    "Envy"
    "reflect.png"
    (Cards.grudge, Cards.overwhelm, Cards.potion, Cards.reflect)


sloth :: Rune
sloth =
  Rune
    "Sloth"
    "balance.png"
    (Cards.katana, Cards.curse, Cards.bless, Cards.balance)


greed :: Rune
greed =
  Rune
    "Greed"
    "alchemy.png"
    (Cards.relicblade, Cards.greed, Cards.echo, Cards.alchemy)


-- Characters
allCharacters :: [Character]
allCharacters = [
    catherine
  , miguel
  ]


catherine :: Character
catherine =
  Character
    "Catherine"
    wrath
    lust
    envy


miguel :: Character
miguel =
  Character
    "Miguel"
    pride
    gluttony
    sloth


characterCards :: Character -> Deck
characterCards Character{ character_rune_a, character_rune_b, character_rune_c } =
  concat $ (\(p, q, r, s) -> [p, q, r, s]) <$>
    [rune_cards character_rune_a, rune_cards character_rune_b, rune_cards character_rune_c]
