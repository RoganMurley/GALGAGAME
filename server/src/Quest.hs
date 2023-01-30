{-# LANGUAGE LambdaCase #-}

module Quest where

import CardAnim (CardAnim (..))
import Data.String.Conversions (cs)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text, toUpper)
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Stats.Experience (Experience)
import Model (Model(..))
import Card (Aspect(..), Card(..), Suit(..), aspectText, allAspects, cardName)
import qualified Cards
import StackCard (StackCard(..))
import qualified DSL.Alpha as Alpha
import {-# SOURCE #-} DeckBuilding (Rune, getRuneAspect)
import qualified ModelDiff
import Wheel (Wheel(..))

data Quest = Quest
  { quest_id :: Text,
    quest_name :: Text,
    quest_desc :: Text,
    quest_xp :: Experience,
    quest_eligible :: Set Rune -> Bool,
    quest_pattern :: Model -> [ResolveData] -> Bool
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

test :: Set Quest -> Model -> [ResolveData] -> Set Quest
test quests initial res = Set.filter (\Quest {quest_pattern} -> not $ quest_pattern initial res) quests

resZip :: Model -> [ResolveData] -> [(Model, ResolveData)]
resZip _ [] = []
resZip model (r:rs) = (newModel, r) : resZip newModel rs
  where
    newModel :: Model
    newModel = ModelDiff.update model (resolveData_diff r)

cardActive :: Model -> Card -> Bool
cardActive model card =
  case model of
    Model { model_stack = Wheel { wheel_0 = Just StackCard { stackcard_card = activeCard } } } ->
      activeCard == card
    _ ->
      False

bigDamageQuest :: Quest
bigDamageQuest =
  Quest
    { quest_id = "50dmg",
      quest_name = "THE BIG ONE",
      quest_desc = "Do exactly 50 damage",
      quest_xp = 500,
      quest_eligible = const True,
      quest_pattern = \_ res ->
        any
          ( \case
              ResolveData {resolveData_anim = Just (Hurt PlayerB 50 _)} ->
                True
              _ ->
                False
          ) res
    }

cardWinQuest :: Suit -> Aspect -> Quest
cardWinQuest suit aspect =
  let
    card = Cards.getCard aspect suit
    name = cardName aspect suit
  in
  Quest
    { quest_id = name <> "Win",
      quest_name = name <> "PROPHECY",
      quest_desc = "Win with damage from " <> name,
      quest_xp = 500,
      quest_eligible = any (\rune -> getRuneAspect rune == aspect),
      quest_pattern = \initial res ->
        any
          ( \case
              (model, ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))}) ->
                cardActive model card
              _ ->
                False
          ) (resZip initial res)
    }

winAspect :: Aspect -> Quest
winAspect aspect =
  Quest
    { quest_id = "win" <> aspectText aspect,
      quest_name = aspectText aspect <> "PROPHECY",
      quest_desc = "Win with " <> toUpper (aspectText aspect),
      quest_xp = 250,
      quest_eligible = any (\rune -> getRuneAspect rune == aspect), 
      quest_pattern = \initial res ->
        let
          isAspect =
            Alpha.evalI initial $ do
              deck <- Alpha.getDeck PlayerA
              return (any (\Card{card_aspect} -> card_aspect == aspect) deck)
        in didWin res && isAspect
    }

aspectQuests :: [Quest]
aspectQuests = winAspect <$> allAspects

swordQuests :: [Quest]
swordQuests = cardWinQuest Sword <$> allAspects

wandQuests :: [Quest]
wandQuests = cardWinQuest Wand <$> allAspects

didWin :: [ResolveData] -> Bool
didWin =
  any (\case
      ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))} ->
        True
      _ ->
        False
  )

allQuests :: [Quest]
allQuests = [bigDamageQuest] ++ aspectQuests ++ swordQuests ++ wandQuests

eligibleQuests :: Set Rune -> [Quest]
eligibleQuests unlocks = filter (\Quest{ quest_eligible } -> quest_eligible unlocks) allQuests

questsById :: Map Text Quest
questsById = Map.fromList $ fmap (\quest -> (quest_id quest, quest)) allQuests

getById :: Text -> Maybe Quest
getById qid = Map.lookup qid questsById

setup :: [Quest] -> [Quest]
-- setup [] = allQuests
-- setup quests = quests
setup = id
-- setup = const swordQuests
