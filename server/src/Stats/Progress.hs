{-# LANGUAGE DeriveGeneric #-}

module Stats.Progress where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import {-# SOURCE #-} DeckBuilding (Rune, getRuneByName, getRuneName)
import Stats.Experience (Experience)
import qualified Quest
import Quest (Quest)

-- Progress
data Progress = Progress
  { progress_xp :: Experience,
    progress_unlocks :: Set Rune,
    progress_events :: Set Text,
    progress_quests :: Set Quest
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
        progress_quests = progress_quests a <> progress_quests b
      }

instance Monoid Progress where
  mappend = (<>)
  mempty =
    Progress
      { progress_xp = 0,
        progress_unlocks = Set.empty,
        progress_events = Set.empty,
        progress_quests = Set.empty
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

isTutorialProgress :: Progress -> Bool
isTutorialProgress Progress {progress_events} =
  not $ Set.member "tutorial-complete" progress_events

-- Partial progress for saving to JSON
data PartialProgress = PartialProgress
  { partialprogress_unlocks :: [Text],
    partialprogress_events :: [Text],
    partialprogress_quests :: [Text]
  }

instance ToJSON PartialProgress where
  toJSON p =
    object
      [ "unlocks" .= partialprogress_unlocks p,
        "events" .= partialprogress_events p,
        "quests" .= partialprogress_quests p
      ]

instance FromJSON PartialProgress where
  parseJSON =
    withObject "PartialProgress" $ \o ->
      makePartial
        <$> o .: "unlocks"
        <*> o .: "events"
        <*> o .:? "quests"

makePartial :: [Text] -> [Text] -> Maybe [Text] -> PartialProgress
makePartial unlocks events mQuests = PartialProgress unlocks events quests
  where
    quests = fromMaybe [] mQuests

toPartial :: Progress -> PartialProgress
toPartial Progress {progress_unlocks, progress_events, progress_quests} =
  PartialProgress
    { partialprogress_unlocks = getRuneName <$> Set.toList progress_unlocks,
      partialprogress_events = Set.toList progress_events,
      partialprogress_quests = Quest.quest_name <$> Set.toList progress_quests
    }

fromPartial :: PartialProgress -> Experience -> Progress
fromPartial PartialProgress {partialprogress_unlocks, partialprogress_events, partialprogress_quests} xp =
  Progress
    { progress_xp = xp,
      progress_unlocks = unlocks,
      progress_events = events,
      progress_quests = quests
    }
  where
    unlocks = Set.fromList $ catMaybes $ getRuneByName . cs <$> partialprogress_unlocks
    events = Set.fromList partialprogress_events
    quests = Set.fromList $ catMaybes $ Quest.getByName . cs <$> partialprogress_quests
