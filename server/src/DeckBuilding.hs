module DeckBuilding where

import Card (Card (..))
import qualified Cards
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Either (isRight)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Life (Life, initMaxLife)
import Mirror (Mirror (..))
import Model (Deck)
import Player (WhichPlayer (..))
import Stats.Stats (Experience, levelToExperience)

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

data ChosenCharacter = ChosenCharacter Character
  deriving (Eq, Show)

instance ToJSON ChosenCharacter where
  toJSON (ChosenCharacter character) =
    object ["chosen" .= character]

data UnchosenCharacter = UnchosenCharacter (Maybe Character)
  deriving (Eq, Show)

instance ToJSON UnchosenCharacter where
  toJSON (UnchosenCharacter character) =
    object ["unchosen" .= character]

-- DeckBuilding
data DeckBuilding = DeckBuilding
  { deckbuilding_pa :: Either UnchosenCharacter ChosenCharacter,
    deckbuilding_pb :: Either UnchosenCharacter ChosenCharacter
  }
  deriving (Eq, Show)

instance ToJSON DeckBuilding where
  toJSON DeckBuilding {deckbuilding_pa} =
    case deckbuilding_pa of
      Left unchosen ->
        object
          [ "character" .= unchosen,
            "all_runes" .= mainRunes
          ]
      Right chosen ->
        object
          [ "character" .= chosen,
            "all_runes" .= mainRunes
          ]

instance Mirror DeckBuilding where
  mirror (DeckBuilding pa pb) = DeckBuilding pb pa

initDeckBuilding :: Either UnchosenCharacter ChosenCharacter -> Either UnchosenCharacter ChosenCharacter -> DeckBuilding
initDeckBuilding = DeckBuilding

selectCharacter :: DeckBuilding -> WhichPlayer -> Character -> DeckBuilding
selectCharacter deckModel which character =
  case which of
    PlayerA ->
      DeckBuilding
        { deckbuilding_pa = Right . ChosenCharacter $ character,
          deckbuilding_pb = deckbuilding_pb deckModel
        }
    PlayerB ->
      DeckBuilding
        { deckbuilding_pa = deckbuilding_pa deckModel,
          deckbuilding_pb = Right . ChosenCharacter $ character
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

choiceToCharacter :: CharacterChoice -> Experience -> Either Text Character
choiceToCharacter CharacterChoice {choice_ra, choice_rb, choice_rc} xp =
  let uniqueChoices :: Bool
      uniqueChoices = Set.size (Set.fromList [choice_ra, choice_rb, choice_rc]) == 3
      makeCharacter :: Rune -> Rune -> Rune -> Character
      makeCharacter choicePa choicePb choicePc =
        Character (Left (choicePa, choicePb, choicePc)) initMaxLife
   in if uniqueChoices
        then
          makeCharacter
            <$> getRune choice_ra xp
            <*> getRune choice_rb xp
            <*> getRune choice_rc xp
        else Left "Rune choices were not unique"

getRune :: Text -> Experience -> Either Text Rune
getRune name xp =
  case find (\Rune {rune_name} -> rune_name == name) mainRunes of
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
    emptyRune
  ]

allRunes :: [Rune]
allRunes =
  mainRunes
    ++ [ bloodRune,
         crownRune,
         abyssRune
       ]

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
    0

crownRune :: Rune
crownRune =
  Rune
    "CROWN"
    "cards/crown/coin.png"
    (Cards.crownSword, Cards.crownWand, Cards.crownGrail, Cards.crownCoin)
    0

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
