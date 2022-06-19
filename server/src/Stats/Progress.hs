{-# LANGUAGE DeriveGeneric #-}

module Stats.Progress where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding, object, withObject, (.:), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import DeckBuilding (Rune (..), getRuneByName)
import GHC.Generics
import Stats.Experience (Experience)

-- Progress
data Progress = Progress
  { progress_xp :: Experience,
    progress_unlocks :: [Rune],
    progress_events :: [Event]
  }
  deriving (Eq, Show)

instance ToJSON Progress where
  toJSON progress = toJSON $ toPartial progress

initialProgress :: Progress
initialProgress =
  Progress
    { progress_xp = 0,
      progress_unlocks = [],
      progress_events = []
    }

getXp :: Progress -> Experience
getXp = progress_xp

makeCpuProgress :: Experience -> Progress
makeCpuProgress xp = initialProgress {progress_xp = xp}

-- Partial progress for saving to JSON
data PartialProgress = PartialProgress
  { partialprogress_unlocks :: [Rune],
    partialprogress_events :: [Event]
  }
  deriving (Eq, Generic, Show)

instance ToJSON PartialProgress where
  toJSON p =
    object
      [ "unlocks" .= (rune_name <$> partialprogress_unlocks p),
        "events" .= partialprogress_events p
      ]

instance FromJSON PartialProgress where
  parseJSON =
    withObject "PartialProgress" $ \o ->
      loadProgress
        <$> o .: "unlocks"
        <*> o .: "events"
    where
      loadProgress :: [Text] -> [Event] -> PartialProgress
      loadProgress runeNames events =
        PartialProgress
          (catMaybes $ getRuneByName <$> runeNames)
          events

toPartial :: Progress -> PartialProgress
toPartial Progress {progress_unlocks, progress_events} =
  PartialProgress
    { partialprogress_unlocks = progress_unlocks,
      partialprogress_events = progress_events
    }

fromPartial :: PartialProgress -> Experience -> Progress
fromPartial PartialProgress {partialprogress_unlocks, partialprogress_events} xp =
  Progress
    { progress_xp = xp,
      progress_unlocks = partialprogress_unlocks,
      progress_events = partialprogress_events
    }

-- Events
data Event = TutorialEvent
  deriving (Eq, Generic, Show)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Event
