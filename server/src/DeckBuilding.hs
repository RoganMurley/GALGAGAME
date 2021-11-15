module DeckBuilding where

import Card (Card(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.List (find)
import Data.Either (isRight)
import Data.Text (Text)
import Life (Life, initMaxLife)
import Mirror (Mirror(..))
import Model (Deck)
import Player (WhichPlayer(..))
import Util (Gen, shuffle)

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
  { character_choice :: Either (Rune, Rune, Rune) Deck
  , character_maxLife :: Life
  }
  deriving (Eq, Show)


instance ToJSON Character where
  toJSON (Character (Left (runeA, runeB, runeC)) _) =
    object
      [ "choice" .= (
        object [
          "rune_a" .= runeA
        , "rune_b" .= runeB
        , "rune_c" .= runeC
        ]
      )
      ]
  toJSON (Character _ _) =
    object
      [ "choice" .= (Nothing :: Maybe (Rune, Rune, Rune))
      ]


data ChosenCharacter = ChosenCharacter Character
  deriving (Eq, Show)


instance ToJSON ChosenCharacter where
  toJSON (ChosenCharacter character) =
    object ["chosen" .= character]


data UnchosenCharacter = UnchosenCharacter Character
  deriving (Eq, Show)


instance ToJSON UnchosenCharacter where
  toJSON (UnchosenCharacter character) =
    object ["unchosen" .= character]


-- DeckBuilding
data DeckBuilding =
  DeckBuilding
  { deckbuilding_pa :: Either UnchosenCharacter ChosenCharacter
  , deckbuilding_pb :: Either UnchosenCharacter ChosenCharacter
  } deriving (Eq, Show)


instance ToJSON DeckBuilding where
  toJSON DeckBuilding{ deckbuilding_pa } =
    case deckbuilding_pa of
      Left unchosen ->
        object
          [ "character" .= unchosen
          , "all_runes" .= mainRunes
          ]
      Right chosen ->
        object
          [ "character" .= chosen
          , "all_runes" .= mainRunes
          ]


instance Mirror DeckBuilding where
  mirror (DeckBuilding pa pb) = DeckBuilding pb pa


initDeckBuilding :: Either UnchosenCharacter ChosenCharacter -> Either UnchosenCharacter ChosenCharacter -> DeckBuilding
initDeckBuilding = DeckBuilding


selectCharacter :: DeckBuilding -> WhichPlayer -> Character -> DeckBuilding
selectCharacter deckModel which character =
  case which of
    PlayerA ->
      DeckBuilding {
        deckbuilding_pa = Right . ChosenCharacter $ character
      , deckbuilding_pb = deckbuilding_pb deckModel
      }
    PlayerB ->
      DeckBuilding {
        deckbuilding_pa = deckbuilding_pa deckModel
      , deckbuilding_pb = Right . ChosenCharacter $ character
      }


isReady :: DeckBuilding -> WhichPlayer -> Bool
isReady deckModel which =
  case which of
    PlayerA ->
      isRight $ deckbuilding_pa deckModel
    PlayerB ->
      isRight $ deckbuilding_pb deckModel


-- CharacterChoice
data CharacterChoice = CharacterChoice
  { choice_ra   :: Text
  , choice_rb   :: Text
  , choice_rc   :: Text
  } deriving (Eq, Show)


instance FromJSON CharacterChoice where
  parseJSON =
    withObject "CharacterChoice" $
    \o ->
      CharacterChoice
        <$> o .: "rune_a"
        <*> o .: "rune_b"
        <*> o .: "rune_c"


choiceToCharacter :: CharacterChoice -> Either Text Character
choiceToCharacter CharacterChoice{choice_ra, choice_rb, choice_rc} =
  let
    uniqueChoices :: Bool
    uniqueChoices = Set.size (Set.fromList [choice_ra, choice_rb, choice_rc]) == 3
    makeCharacter :: Rune -> Rune -> Rune -> Character
    makeCharacter choicePa choicePb choicePc =
      Character (Left (choicePa, choicePb, choicePc)) initMaxLife
  in
  if uniqueChoices then
    makeCharacter
      <$> getRune choice_ra
      <*> getRune choice_rb
      <*> getRune choice_rc
  else
    Left "Rune choices were not unique"


getRune :: Text -> Either Text Rune
getRune name =
  case find (\Rune{rune_name} -> rune_name == name) mainRunes of
    Just rune ->
      Right rune
    Nothing ->
      Left "Invalid rune name"


-- RUNES
mainRunes :: [Rune]
mainRunes =
  [ blazeRune
  , tideRune
  , heavenRune
  , shroomRune
  , mirageRune
  , mirrorRune
  , dualityRune
  , alchemyRune
  , morphRune
  , feverRune
  , possibilityRune
  ]

allRunes :: [Rune]
allRunes =
  mainRunes ++
  [ bloodRune
  , crownRune
  , abyssRune
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

abyssRune :: Rune
abyssRune =
  Rune
    "ABYSS"
    "cards/morph/coin.png"
    (Cards.abyssSword, Cards.abyssWand, Cards.abyssGrail, Cards.abyssSword)

feverRune :: Rune
feverRune =
  Rune
    "FEVER"
    "cards/fever/coin.png"
    (Cards.feverSword, Cards.feverWand, Cards.feverGrail, Cards.feverCoin)

possibilityRune :: Rune
possibilityRune =
  Rune
    "POSSIBILITY"
    "cards/possibility/coin.png"
    (Cards.possibilitySword, Cards.possibilityWand, Cards.possibilityGrail, Cards.possibilityCoin)


characterCards :: Character -> Deck
characterCards Character{ character_choice } =
  case character_choice of
    Left (runeA, runeB, runeC) ->
      concat $ (\(p, q, r, s) -> [p, q, r, s]) <$>
        [rune_cards runeA, rune_cards runeB, rune_cards runeC] >>= replicate 3
    Right deck ->
      deck


randomRunes :: Gen -> (Rune, Rune, Rune)
randomRunes gen =
  case shuffle gen mainRunes of
    runeA : runeB : runeC : _ ->
      (runeA, runeB, runeC)
    _ ->
      error "not enough runes to randomise"
