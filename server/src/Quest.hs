{-# LANGUAGE LambdaCase #-}

module Quest where

import CardAnim (CardAnim (..))
import Data.String.Conversions (cs)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Stats.Experience (Experience)

data Quest = Quest
  { quest_id :: Text,
    quest_name :: Text,
    quest_desc :: Text,
    quest_xp :: Experience,
    quest_pattern :: [ResolveData] -> Bool
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

test :: Set Quest -> [ResolveData] -> Set Quest
test quests res = Set.filter (\Quest {quest_pattern} -> not $ quest_pattern res) quests

bigDamageQuest :: Quest
bigDamageQuest =
  Quest
    { quest_id = "50dmg",
      quest_name = "THE BIG ONE",
      quest_desc = "Do exactly 50 damage",
      quest_xp = 1000,
      quest_pattern =
        any
          ( \case
              ResolveData {resolveData_anim = Just (Hurt PlayerB 50 _)} ->
                True
              _ ->
                False
          )
    }

winQuest :: Quest
winQuest =
  Quest
    { quest_id = "win",
      quest_name = "VICTORIOUS",
      quest_desc = "Win a game",
      quest_xp = 100,
      quest_pattern =
        any
          ( \case
              ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))} ->
                True
              _ ->
                False
          )
    }

loseQuest :: Quest
loseQuest =
  Quest
    { quest_id = "lose",
      quest_name = "LOSER",
      quest_desc = "Lose a game",
      quest_xp = 100,
      quest_pattern =
        any
          ( \case
              ResolveData {resolveData_anim = Just (GameEnd (Just PlayerB))} ->
                True
              _ ->
                False
          )
    }

allQuests :: [Quest]
allQuests = [bigDamageQuest, winQuest, loseQuest]

questsById :: Map Text Quest
questsById = Map.fromList $ fmap (\quest -> (quest_id quest, quest)) allQuests

getById :: Text -> Maybe Quest
getById qid = Map.lookup qid questsById

setup :: [Quest] -> [Quest]
-- setup [] = allQuests
-- setup quests = quests
setup _ = allQuests
