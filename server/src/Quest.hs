{-# LANGUAGE LambdaCase #-}

module Quest where

import CardAnim (CardAnim (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import qualified Data.Map as Map
import Data.Map (Map)

data Quest = Quest
  { quest_name :: Text,
    quest_desc :: Text,
    quest_pattern :: [ResolveData] -> Bool
  }

instance Show Quest where
  show quest = cs $ quest_name quest

instance Eq Quest where
  (==) a b = quest_name a == quest_name b

instance Ord Quest where
  a `compare` b = quest_name a `compare` quest_name b

test :: [Quest] -> [ResolveData] -> [Quest]
test quests res = filter (\Quest {quest_pattern} -> quest_pattern res) quests


bigDamageQuest :: Quest
bigDamageQuest =
  Quest
    { quest_name = "THE BIG ONE",
      quest_desc = "Do exactly 50 damage",
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
    { quest_name = "VICTORIOUS",
      quest_desc = "Win a game",
      quest_pattern =
        any
          ( \case
              ResolveData {resolveData_anim = Just (GameEnd (Just PlayerA))} ->
                True
              _ ->
                False
          )
    }

allQuests :: [Quest]
allQuests = [bigDamageQuest, winQuest]

questsByName :: Map Text Quest
questsByName = Map.fromList $ fmap (\quest -> (quest_name quest, quest)) allQuests

getByName :: Text -> Maybe Quest
getByName name = Map.lookup name questsByName
