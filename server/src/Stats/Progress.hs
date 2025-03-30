{-# LANGUAGE DeriveGeneric #-}

module Stats.Progress where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime)
import {-# SOURCE #-} DeckBuilding (Rune, getRuneByName, getRuneName)
import Quest (Quest)
import Quest qualified
import Stats.Experience (Experience)

-- Progress
data Progress = Progress
  { progress_xp :: Experience,
    progress_unlocks :: Set Rune,
    progress_events :: Set Text,
    progress_quests :: Set Quest,
    progress_questupdate :: Maybe UTCTime
  }
  deriving (Show)

instance ToJSON Progress where
  toJSON progress = toJSON $ toPartial progress

instance Semigroup Progress where
  a <> b =
    Progress
      { progress_xp = progress_xp a + progress_xp b,
        progress_unlocks = progress_unlocks a <> progress_unlocks b,
        progress_events = progress_events a <> progress_events b,
        progress_quests = progress_quests a <> progress_quests b,
        progress_questupdate = progress_questupdate a
      }

instance Monoid Progress where
  mappend = (<>)
  mempty =
    Progress
      { progress_xp = 0,
        progress_unlocks = Set.empty,
        progress_events = Set.empty,
        progress_quests = Set.empty,
        progress_questupdate = Nothing
      }

initialProgress :: Progress
initialProgress = mempty {progress_unlocks = unlocks}
  where
    unlocks =
      Set.fromList $
        catMaybes
          [ getRuneByName "ANGEL",
            getRuneByName "WATER",
            getRuneByName "SHROOM",
            getRuneByName "FIRE"
          ]

getXp :: Progress -> Experience
getXp = progress_xp

isUnlocked :: Rune -> Progress -> Bool
isUnlocked rune progress =
  Set.member rune (progress_unlocks progress)

unlockNames :: Progress -> [Text]
unlockNames Progress {progress_unlocks} = getRuneName <$> Set.toList progress_unlocks

questDescs :: Progress -> [Text]
questDescs Progress {progress_quests} = Quest.quest_desc <$> Set.toList progress_quests

isTutorialProgress :: Progress -> Bool
isTutorialProgress Progress {progress_events} =
  not $ Set.member "tutorial-complete" progress_events

-- Partial progress for saving to JSON
data PartialProgress = PartialProgress
  { partialprogress_unlocks :: [Text],
    partialprogress_events :: [Text],
    partialprogress_quests :: [Text],
    partialprogress_questupdate :: Maybe UTCTime
  }

instance ToJSON PartialProgress where
  toJSON p =
    object
      [ "unlocks" .= partialprogress_unlocks p,
        "events" .= partialprogress_events p,
        "quests" .= partialprogress_quests p,
        "questupdate" .= partialprogress_questupdate p
      ]

instance FromJSON PartialProgress where
  parseJSON =
    withObject "PartialProgress" $ \o ->
      makePartial
        <$> o .: "unlocks"
        <*> o .: "events"
        <*> o .:? "quests"
        <*> o .:? "questupdate"

makePartial :: [Text] -> [Text] -> Maybe [Text] -> Maybe UTCTime -> PartialProgress
makePartial unlocks events mQuests = PartialProgress unlocks events quests
  where
    quests = fromMaybe [] mQuests

toPartial :: Progress -> PartialProgress
toPartial Progress {progress_unlocks, progress_events, progress_quests, progress_questupdate} =
  PartialProgress
    { partialprogress_unlocks = getRuneName <$> Set.toList progress_unlocks,
      partialprogress_events = Set.toList progress_events,
      partialprogress_quests = Quest.quest_id <$> Set.toList progress_quests,
      partialprogress_questupdate = progress_questupdate
    }

fromPartial :: PartialProgress -> Experience -> Progress
fromPartial PartialProgress {partialprogress_unlocks, partialprogress_events, partialprogress_quests, partialprogress_questupdate} xp =
  Progress
    { progress_xp = xp,
      progress_unlocks = unlocks,
      progress_events = events,
      progress_quests = quests,
      progress_questupdate = partialprogress_questupdate
    }
  where
    unlocks = Set.fromList $ catMaybes $ getRuneByName . cs <$> partialprogress_unlocks
    events = Set.fromList partialprogress_events
    quests = Set.fromList $ Quest.setup $ catMaybes $ Quest.getById . cs <$> partialprogress_quests
