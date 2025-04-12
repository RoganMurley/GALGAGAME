module DeckBuilding where

import Card (Aspect (..), Card (..))
import Cards qualified
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Either (isRight)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set as Set
import Data.Text (Text)
import HandCard (HandCard (..))
import Life (Life, initMaxLife)
import Mirror (Mirror (..))
import Model (Deck)
import Player (WhichPlayer (..))
import Stats.Experience (Experience, levelToExperience)
import Stats.Progress (Progress (..), isUnlocked)

-- Rune
data Rune = Rune
  { rune_name :: Text,
    rune_aspect :: Aspect,
    rune_img :: Text,
    rune_cards :: RuneCards,
    rune_xp :: Experience
  }
  deriving (Eq, Show)

instance Ord Rune where
  a `compare` b = rune_name a `compare` rune_name b

instance ToJSON Rune where
  toJSON Rune {rune_name, rune_img, rune_cards, rune_xp} =
    object
      [ "name" .= rune_name,
        "img_url" .= rune_img,
        "cards" .= rune_cards,
        "xp" .= rune_xp
      ]

type RuneCards = (Card, Card, Card, Card)

getRuneName :: Rune -> Text
getRuneName = rune_name

getRuneAspect :: Rune -> Aspect
getRuneAspect = rune_aspect

-- Character
data Character = Character
  { character_choice :: Either (Rune, Rune, Rune) Deck,
    character_maxLife :: Life
  }
  deriving (Eq, Show)

instance ToJSON Character where
  toJSON (Character (Left (runeA, runeB, runeC)) _) =
    object
      [ "choice"
          .= object
            [ "rune_a" .= runeA,
              "rune_b" .= runeB,
              "rune_c" .= runeC
            ]
      ]
  toJSON (Character _ _) =
    object
      [ "choice" .= (Nothing :: Maybe (Rune, Rune, Rune))
      ]

newtype ChosenCharacter = ChosenCharacter (Maybe Character)
  deriving (Eq, Show)

instance ToJSON ChosenCharacter where
  toJSON (ChosenCharacter character) =
    object ["chosen" .= character]

newtype UnchosenCharacter = UnchosenCharacter (Maybe Character)
  deriving (Eq, Show)

instance ToJSON UnchosenCharacter where
  toJSON (UnchosenCharacter character) =
    object ["unchosen" .= character]

-- DeckBuilding
data DeckBuilding = DeckBuilding
  { deckbuilding_pa :: Either UnchosenCharacter ChosenCharacter,
    deckbuilding_runes_pa :: [Rune],
    deckbuilding_pb :: Either UnchosenCharacter ChosenCharacter,
    deckbuilding_runes_pb :: [Rune]
  }
  deriving (Eq, Show)

instance ToJSON DeckBuilding where
  toJSON DeckBuilding {deckbuilding_pa, deckbuilding_runes_pa} =
    case deckbuilding_pa of
      Left unchosen ->
        object
          [ "character" .= unchosen,
            "all_runes" .= deckbuilding_runes_pa
          ]
      Right chosen ->
        object
          [ "character" .= chosen,
            "all_runes" .= deckbuilding_runes_pa
          ]

instance Mirror DeckBuilding where
  mirror (DeckBuilding pa runesPa pb runesPb) = DeckBuilding pb runesPb pa runesPa

initDeckBuilding :: (Bool, Bool) -> Either UnchosenCharacter ChosenCharacter -> Either UnchosenCharacter ChosenCharacter -> DeckBuilding
initDeckBuilding (paSuper, pbSuper) choicePa choicePb =
  DeckBuilding
    { deckbuilding_pa = choicePa,
      deckbuilding_runes_pa = if paSuper then superRunes else mainRunes,
      deckbuilding_pb = choicePb,
      deckbuilding_runes_pb = if pbSuper then superRunes else mainRunes
    }

selectCharacter :: DeckBuilding -> WhichPlayer -> Character -> DeckBuilding
selectCharacter deckModel which character =
  case which of
    PlayerA ->
      deckModel
        { deckbuilding_pa = Right . ChosenCharacter . Just $ character
        }
    PlayerB ->
      deckModel
        { deckbuilding_pb = Right . ChosenCharacter . Just $ character
        }

isReady :: DeckBuilding -> WhichPlayer -> Bool
isReady deckModel which =
  case which of
    PlayerA ->
      isRight $ deckbuilding_pa deckModel
    PlayerB ->
      isRight $ deckbuilding_pb deckModel

getSelectableRunes :: WhichPlayer -> DeckBuilding -> [Rune]
getSelectableRunes PlayerA = deckbuilding_runes_pa
getSelectableRunes PlayerB = deckbuilding_runes_pb

-- CharacterChoice
data CharacterChoice = CharacterChoice
  { choice_ra :: Text,
    choice_rb :: Text,
    choice_rc :: Text
  }
  deriving (Eq, Show)

instance FromJSON CharacterChoice where
  parseJSON =
    withObject "CharacterChoice" $
      \o ->
        CharacterChoice
          <$> o .: "rune_a"
          <*> o .: "rune_b"
          <*> o .: "rune_c"

choiceToCharacter :: CharacterChoice -> [Rune] -> Progress -> Either Text Character
choiceToCharacter CharacterChoice {choice_ra, choice_rb, choice_rc} runes progress =
  let uniqueChoices :: Bool
      uniqueChoices = Set.size (Set.fromList [choice_ra, choice_rb, choice_rc]) == 3
      makeCharacter :: Rune -> Rune -> Rune -> Character
      makeCharacter choicePa choicePb choicePc =
        Character (Left (choicePa, choicePb, choicePc)) initMaxLife
   in if uniqueChoices
        then
          makeCharacter
            <$> getRune choice_ra runes progress
            <*> getRune choice_rb runes progress
            <*> getRune choice_rc runes progress
        else Left "Rune choices were not unique"

getRune :: Text -> [Rune] -> Progress -> Either Text Rune
getRune name runes progress =
  case find (\Rune {rune_name} -> rune_name == name) runes of
    Just rune ->
      if isUnlocked rune progress
        then Right rune
        else Left $ "Locked rune"
    Nothing ->
      Left "Invalid rune name"

-- RUNES
mainRunes :: [Rune]
mainRunes =
  [ fireRune,
    waterRune,
    angelRune,
    shroomRune,
    mirrorRune,
    dualityRune,
    goldRune,
    clayRune,
    eyeRune,
    feverRune,
    voidRune,
    bloodRune,
    glassRune,
    plasticRune,
    trickRune,
    devilRune
  ]

superRunes :: [Rune]
superRunes = mainRunes ++ [mercyRune]

allRunes :: [Rune]
allRunes = superRunes

waterRune :: Rune
waterRune =
  Rune
    "WATER"
    Water
    "cards/water/coin.png"
    (Cards.waterSword, Cards.waterWand, Cards.waterCup, Cards.waterCoin)
    (levelToExperience 1)

fireRune :: Rune
fireRune =
  Rune
    "FIRE"
    Fire
    "cards/fire/coin.png"
    (Cards.fireSword, Cards.fireWand, Cards.fireCup, Cards.fireCoin)
    (levelToExperience 1)

angelRune :: Rune
angelRune =
  Rune
    "ANGEL"
    Angel
    "cards/angel/coin.png"
    (Cards.angelSword, Cards.angelWand, Cards.angelCup, Cards.angelCoin)
    (levelToExperience 1)

shroomRune :: Rune
shroomRune =
  Rune
    "SHROOM"
    Shroom
    "cards/shroom/coin.png"
    (Cards.shroomSword, Cards.shroomWand, Cards.shroomCup, Cards.shroomCoin)
    (levelToExperience 1)

mirrorRune :: Rune
mirrorRune =
  Rune
    "MIRROR"
    Mirror
    "cards/mirror/coin.png"
    (Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorCup, Cards.mirrorCoin)
    (levelToExperience 2)

goldRune :: Rune
goldRune =
  Rune
    "GOLD"
    Gold
    "cards/gold/coin.png"
    (Cards.goldSword, Cards.goldWand, Cards.goldCup, Cards.goldCoin)
    (levelToExperience 3)

voidRune :: Rune
voidRune =
  Rune
    "VOID"
    Void
    "cards/void/coin.png"
    (Cards.voidSword, Cards.voidWand, Cards.voidCup, Cards.voidCoin)
    (levelToExperience 4)

dualityRune :: Rune
dualityRune =
  Rune
    "DUALITY"
    Duality
    "cards/duality/coin.png"
    (Cards.dualitySword, Cards.dualityWand, Cards.dualityCup, Cards.dualityCoin)
    (levelToExperience 5)

eyeRune :: Rune
eyeRune =
  Rune
    "EYE"
    Eye
    "cards/eye/coin.png"
    (Cards.eyeSword, Cards.eyeWand, Cards.eyeCup, Cards.eyeCoin)
    (levelToExperience 6)

feverRune :: Rune
feverRune =
  Rune
    "FEVER"
    Fever
    "cards/fever/coin.png"
    (Cards.feverSword, Cards.feverWand, Cards.feverCup, Cards.feverCoin)
    (levelToExperience 7)

clayRune :: Rune
clayRune =
  Rune
    "CLAY"
    Clay
    "cards/clay/coin.png"
    (Cards.claySword, Cards.clayWand, Cards.clayCup, Cards.clayCoin)
    (levelToExperience 8)

bloodRune :: Rune
bloodRune =
  Rune
    "BLOOD"
    Blood
    "cards/blood/coin.png"
    (Cards.bloodSword, Cards.bloodWand, Cards.bloodCup, Cards.bloodCoin)
    (levelToExperience 9)

glassRune :: Rune
glassRune =
  Rune
    "GLASS"
    Glass
    "cards/glass/coin.png"
    (Cards.glassSword, Cards.glassWand, Cards.glassCup, Cards.glassCoin)
    (levelToExperience 10)

plasticRune :: Rune
plasticRune =
  Rune
    "PLASTIC"
    Plastic
    "cards/plastic/coin.png"
    (Cards.plasticSword, Cards.plasticWand, Cards.plasticCup, Cards.plasticCoin)
    (levelToExperience 11)

trickRune :: Rune
trickRune =
  Rune
    "TRICK"
    Trick
    "cards/trick/coin.png"
    (Cards.trickSword, Cards.trickWand, Cards.trickCup, Cards.trickCoin)
    (levelToExperience 12)

devilRune :: Rune
devilRune =
  Rune
    "DEVIL"
    Devil
    "cards/devil/coin.png"
    (Cards.devilSword, Cards.devilWand, Cards.devilCup, Cards.devilCoin)
    (levelToExperience 13)

mercyRune :: Rune
mercyRune =
  Rune
    "MERCY"
    Mercy
    "cards/mercy/coin.png"
    (Cards.mercySword, Cards.mercyWand, Cards.mercyCup, Cards.mercyCoin)
    (levelToExperience 14)

characterCards :: Character -> Deck
characterCards Character {character_choice} =
  case character_choice of
    Left (runeA, runeB, runeC) ->
      HandCard
        <$> ( concat $
                (\(p, q, r, s) -> [p, q, r, s])
                  <$> [rune_cards runeA, rune_cards runeB, rune_cards runeC]
                  >>= replicate 3
            )
    Right deck ->
      deck

runesByName :: Map Text Rune
runesByName = Map.fromList $ fmap (\rune -> (rune_name rune, rune)) allRunes

getRuneByName :: Text -> Maybe Rune
getRuneByName name = Map.lookup name runesByName
