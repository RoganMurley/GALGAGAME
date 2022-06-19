{-# LANGUAGE DeriveGeneric #-}

module Stats.Progress where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding, object, withObject, (.:), (.=))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import DeckBuilding (Rune (..), getRuneByName)
import GHC.Generics
import Stats.Experience (Experience)

-- Progress
data Progress = Progress
  { progress_xp :: Experience,
    progress_unlocks :: Set Rune,
    progress_events :: Set Event
  }
  deriving (Eq, Show)

instance ToJSON Progress where
  toJSON progress = toJSON $ toPartial progress

initialProgress :: Progress
initialProgress =
  Progress
    { progress_xp = 0,
      progress_unlocks = Set.empty,
      progress_events = Set.empty
    }

getXp :: Progress -> Experience
getXp = progress_xp

makeCpuProgress :: Experience -> Progress
makeCpuProgress xp = initialProgress {progress_xp = xp}

updateProgress :: Progress -> Progress -> Progress
updateProgress a b =
  Progress
    { progress_xp = progress_xp a + progress_xp b,
      progress_unlocks = Set.union (progress_unlocks a) (progress_unlocks b),
      progress_events = Set.union (progress_events a) (progress_events b)
    }

-- Partial progress for saving to JSON
data PartialProgress = PartialProgress
  { partialprogress_unlocks :: Set Rune,
    partialprogress_events :: Set Event
  }
  deriving (Eq, Generic, Show)

instance ToJSON PartialProgress where
  toJSON p =
    object
      [ "unlocks" .= (rune_name <$> Set.toList (partialprogress_unlocks p)),
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
          (Set.fromList $ catMaybes $ getRuneByName <$> runeNames)
          (Set.fromList events)

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
data Event = EventTutorial | MirrorUnlock
  deriving (Eq, Generic, Ord, Show)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Event
