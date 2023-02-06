{-# LANGUAGE LambdaCase #-}

module Quest where

import Card (Aspect (..), Card (..), Suit (..), allAspects, aspectText, cardName, sameCard)
import CardAnim (CardAnim (..))
import qualified Cards
import qualified DSL.Alpha as Alpha
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text, toUpper)
import {-# SOURCE #-} DeckBuilding (Rune, getRuneAspect)
import HandCard (anyCard)
import Model (Model (..))
import qualified ModelDiff
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import StackCard (StackCard (..))
import Stats.Experience (Experience)
import Util (Gen, randomChoice)
import Wheel (Wheel (..))

data Quest = Quest
  { quest_id :: Text,
    quest_name :: Text,
    quest_desc :: Text,
    quest_xp :: Experience,
    quest_rarity :: Rarity,
    quest_eligible :: Set Rune -> Bool,
    quest_pattern :: [Text] -> Model -> [ResolveData] -> Bool
  }

instance Show Quest where
  show quest = cs $ quest_id quest

instance Eq Quest where
  (==) a b = quest_id a == quest_id b

instance Ord Quest where
  a `compare` b = quest_id a `compare` quest_id b

instance ToJSON Quest where
  toJSON Quest {quest_id, quest_name, quest_desc, quest_xp} =
    object
      [ "id" .= quest_id,
        "name" .= quest_name,
        "desc" .= quest_desc,
        "xp" .= quest_xp
      ]

data Rarity = Common | Rare
  deriving (Eq, Show)

test :: [Text] -> Set Quest -> Model -> [ResolveData] -> Set Quest
test tags quests initial res = Set.filter (\Quest {quest_pattern} -> not $ quest_pattern tags initial res) quests

resZip :: Model -> [ResolveData] -> [(Model, ResolveData)]
resZip _ [] = []
resZip model (r : rs) = (newModel, r) : resZip newModel rs
  where
    newModel :: Model
    newModel = ModelDiff.update model (resolveData_diff r)

cardActive :: Model -> Card -> Bool
cardActive model card =
  case model of
    Model {model_stack = Wheel {wheel_0 = Just StackCard {stackcard_card = activeCard}}} ->
      sameCard activeCard card
    _ ->
      False

bigDamageQuest :: Quest
bigDamageQuest =
  Quest
    { quest_id = "50dmg",
      quest_name = "THE BIG ONE",
      quest_desc = "Do exactly 50 damage",
      quest_xp = 250,
      quest_rarity = Rare,
      quest_eligible = const True,
      quest_pattern = \_ _ res ->
        any
          ( \case
              ResolveData {resolveData_anim = Just (Hurt PlayerB 50 _)} ->
                True
              _ ->
                False
          )
          res
    }

cardWinQuest :: Suit -> Aspect -> Quest
cardWinQuest suit aspect =
  let card = Cards.getCard aspect suit
      name = cardName aspect suit
   in Quest
        { quest_id = name <> "Win",
          quest_name = name <> "PROPHECY",
          quest_desc = "Win with damage from " <> name,
          quest_xp = 250,
          quest_rarity = Rare,
          quest_eligible = any (\rune -> getRuneAspect rune == aspect),
          quest_pattern = \_ initial res ->
            any
              ( \case
                  (model, ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))}) ->
                    cardActive model card
                  _ ->
                    False
              )
              (resZip initial res)
        }

winAspect :: Aspect -> Quest
winAspect aspect =
  Quest
    { quest_id = "win" <> aspectText aspect,
      quest_name = aspectText aspect <> "PROPHECY",
      quest_desc = "Win with " <> toUpper (aspectText aspect),
      quest_xp = 250,
      quest_rarity = Common,
      quest_eligible = any (\rune -> getRuneAspect rune == aspect),
      quest_pattern = \_ initial res ->
        let isAspect =
              Alpha.evalI initial $ do
                deck <- Alpha.getDeck PlayerA
                return (any (\Card {card_aspect} -> card_aspect == aspect) (anyCard <$> deck))
         in didWin res && isAspect
    }

playHuman :: Quest
playHuman =
  Quest
    { quest_id = "playHuman",
      quest_name = "HUMANS LIKE US",
      quest_desc = "Play against another human",
      quest_xp = 250,
      quest_rarity = Common,
      quest_eligible = const True,
      quest_pattern = \tags _ _ -> "cpu" `notElem` tags
    }

winHuman :: Quest
winHuman =
  Quest
    { quest_id = "winHuman",
      quest_name = "THE BETTER MAN",
      quest_desc = "Win against another human",
      quest_xp = 250,
      quest_rarity = Rare,
      quest_eligible = const True,
      quest_pattern = \tags _ res -> ("cpu" `notElem` tags) && didWin res
    }

aspectQuests :: [Quest]
aspectQuests = winAspect <$> allAspects

swordQuests :: [Quest]
swordQuests = cardWinQuest Sword <$> allAspects

wandQuests :: [Quest]
wandQuests = cardWinQuest Wand <$> allAspects

didWin :: [ResolveData] -> Bool
didWin =
  any
    ( \case
        ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))} ->
          True
        _ ->
          False
    )

allQuests :: [Quest]
allQuests = [bigDamageQuest, winHuman, playHuman] ++ aspectQuests ++ swordQuests ++ wandQuests

eligibleQuests :: Set Rune -> [Quest]
eligibleQuests unlocks = filter (\Quest {quest_eligible} -> quest_eligible unlocks) allQuests

questsById :: Map Text Quest
questsById = Map.fromList $ fmap (\quest -> (quest_id quest, quest)) allQuests

getById :: Text -> Maybe Quest
getById qid = Map.lookup qid questsById

setup :: [Quest] -> [Quest]
setup = id -- for testing, change this

choose :: Gen -> Set Rune -> Set Quest
choose gen unlocks = Set.singleton . randomChoice gen $ rarityQuests
  where
    quests = Quest.eligibleQuests unlocks
    commonQuests = filter (\Quest {quest_rarity} -> quest_rarity == Common) quests
    rareQuests = filter (\Quest {quest_rarity} -> quest_rarity == Rare) quests
    rarityQuests = randomChoice gen [commonQuests, rareQuests]
