module DeckBuilding where

import Card (Card (..))
import qualified Cards
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Either (isRight)
import Data.List (find)
import Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Life (Life, initMaxLife)
import Mirror (Mirror (..))
import Model (Deck)
import Player (WhichPlayer (..))
import Stats.Experience (Experience, levelToExperience)

-- Rune
data Rune = Rune
  { rune_name :: Text,
    rune_img :: Text,
    rune_cards :: RuneCards,
    rune_xp :: Experience
  }
  deriving (Eq, Show)

instance ToJSON Rune where
  toJSON Rune {rune_name, rune_img, rune_cards, rune_xp} =
    object
      [ "name" .= rune_name,
        "img_url" .= rune_img,
        "cards" .= rune_cards,
        "xp" .= rune_xp
      ]

type RuneCards = (Card, Card, Card, Card)

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

choiceToCharacter :: CharacterChoice -> [Rune] -> Experience -> Either Text Character
choiceToCharacter CharacterChoice {choice_ra, choice_rb, choice_rc} runes xp =
  let uniqueChoices :: Bool
      uniqueChoices = Set.size (Set.fromList [choice_ra, choice_rb, choice_rc]) == 3
      makeCharacter :: Rune -> Rune -> Rune -> Character
      makeCharacter choicePa choicePb choicePc =
        Character (Left (choicePa, choicePb, choicePc)) initMaxLife
   in if uniqueChoices
        then
          makeCharacter
            <$> getRune choice_ra runes xp
            <*> getRune choice_rb runes xp
            <*> getRune choice_rc runes xp
        else Left "Rune choices were not unique"

getRune :: Text -> [Rune] -> Experience -> Either Text Rune
getRune name runes xp =
  case find (\Rune {rune_name} -> rune_name == name) runes of
    Just rune ->
      if rune_xp rune <= xp
        then Right rune
        else Left $ "Locked rune, " <> cs (show (rune_xp rune)) <> ">" <> cs (show xp)
    Nothing ->
      Left "Invalid rune name"

-- RUNES
mainRunes :: [Rune]
mainRunes =
  [ blazeRune,
    tideRune,
    heavenRune,
    shroomRune,
    mirrorRune,
    dualityRune,
    alchemyRune,
    morphRune,
    seerRune,
    feverRune,
    emptyRune,
    bloodRune,
    glassRune,
    myriadRune
  ]

superRunes :: [Rune]
superRunes = mainRunes ++ [cometRune]

allRunes :: [Rune]
allRunes = superRunes ++ [abyssRune]

tideRune :: Rune
tideRune =
  Rune
    "TIDE"
    "cards/tide/coin.png"
    (Cards.tideSword, Cards.tideWand, Cards.tideGrail, Cards.tideCoin)
    (levelToExperience 1)

blazeRune :: Rune
blazeRune =
  Rune
    "BLAZE"
    "cards/blaze/coin.png"
    (Cards.blazeSword, Cards.blazeWand, Cards.blazeGrail, Cards.blazeCoin)
    (levelToExperience 1)

heavenRune :: Rune
heavenRune =
  Rune
    "HEAVEN"
    "cards/heaven/coin.png"
    (Cards.heavenSword, Cards.heavenWand, Cards.heavenGrail, Cards.heavenCoin)
    (levelToExperience 1)

shroomRune :: Rune
shroomRune =
  Rune
    "SHROOM"
    "cards/shroom/coin.png"
    (Cards.shroomSword, Cards.shroomWand, Cards.shroomGrail, Cards.shroomCoin)
    (levelToExperience 1)

mirrorRune :: Rune
mirrorRune =
  Rune
    "MIRROR"
    "cards/mirror/coin.png"
    (Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorGrail, Cards.mirrorCoin)
    (levelToExperience 2)

alchemyRune :: Rune
alchemyRune =
  Rune
    "ALCHEMY"
    "cards/alchemy/coin.png"
    (Cards.alchemySword, Cards.alchemyWand, Cards.alchemyGrail, Cards.alchemyCoin)
    (levelToExperience 3)

emptyRune :: Rune
emptyRune =
  Rune
    "EMPTY"
    "cards/empty/coin.png"
    (Cards.emptySword, Cards.emptyWand, Cards.emptyGrail, Cards.emptyCoin)
    (levelToExperience 4)

dualityRune :: Rune
dualityRune =
  Rune
    "DUALITY"
    "cards/duality/coin.png"
    (Cards.dualitySword, Cards.dualityWand, Cards.dualityGrail, Cards.dualityCoin)
    (levelToExperience 5)

seerRune :: Rune
seerRune =
  Rune
    "SEER"
    "cards/seer/coin.png"
    (Cards.seerSword, Cards.seerWand, Cards.seerGrail, Cards.seerCoin)
    (levelToExperience 6)

feverRune :: Rune
feverRune =
  Rune
    "FEVER"
    "cards/fever/coin.png"
    (Cards.feverSword, Cards.feverWand, Cards.feverGrail, Cards.feverCoin)
    (levelToExperience 7)

morphRune :: Rune
morphRune =
  Rune
    "MORPH"
    "cards/morph/coin.png"
    (Cards.morphSword, Cards.morphWand, Cards.morphGrail, Cards.morphCoin)
    (levelToExperience 8)

bloodRune :: Rune
bloodRune =
  Rune
    "BLOOD"
    "cards/blood/coin.png"
    (Cards.bloodSword, Cards.bloodWand, Cards.bloodGrail, Cards.bloodCoin)
    (levelToExperience 9)

glassRune :: Rune
glassRune =
  Rune
    "GLASS"
    "cards/glass/coin.png"
    (Cards.glassSword, Cards.glassWand, Cards.glassGrail, Cards.glassCoin)
    (levelToExperience 10)

myriadRune :: Rune
myriadRune =
  Rune
    "MYRIAD"
    "cards/myriad/coin.png"
    (Cards.myriadSword, Cards.myriadWand, Cards.myriadGrail, Cards.myriadCoin)
    (levelToExperience 11)

cometRune :: Rune
cometRune =
  Rune
    "COMET"
    "cards/comet/coin.png"
    (Cards.cometSword, Cards.cometWand, Cards.cometGrail, Cards.cometCoin)
    (levelToExperience 12)

abyssRune :: Rune
abyssRune =
  Rune
    "ABYSS"
    "cards/morph/coin.png"
    (Cards.abyssSword, Cards.abyssWand, Cards.abyssGrail, Cards.abyssSword)
    0

characterCards :: Character -> Deck
characterCards Character {character_choice} =
  case character_choice of
    Left (runeA, runeB, runeC) ->
      concat $
        (\(p, q, r, s) -> [p, q, r, s])
          <$> [rune_cards runeA, rune_cards runeB, rune_cards runeC] >>= replicate 3
    Right deck ->
      deck
