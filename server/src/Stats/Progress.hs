{-# LANGUAGE DeriveGeneric #-}

module Stats.Progress where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding, object, withObject, (.:), (.=))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import {-# SOURCE #-} DeckBuilding (Rune, getRuneByName, getRuneName)
import GHC.Generics
import Stats.Experience (Experience)

-- Progress
data Progress = Progress
  { progress_xp :: Experience,
    progress_unlocks :: Set Rune,
    progress_events :: Set Event
  }
  deriving (Show)

instance ToJSON Progress where
  toJSON progress = toJSON $ toPartial progress

instance Semigroup Progress where
  a <> b =
    Progress
      { progress_xp = progress_xp a + progress_xp b,
        progress_unlocks = progress_unlocks a <> progress_unlocks b,
        progress_events = progress_events a <> progress_events b
      }

instance Monoid Progress where
  mappend = (<>)
  mempty =
    Progress
      { progress_xp = 0,
        progress_unlocks = Set.empty,
        progress_events = Set.empty
      }

initialProgress :: Progress
initialProgress = mempty {progress_unlocks = unlocks}
  where
    unlocks =
      Set.fromList $
        catMaybes
          [ getRuneByName "HEAVEN",
            getRuneByName "TIDE",
            getRuneByName "SHROOM",
            getRuneByName "BLAZE"
          ]

getXp :: Progress -> Experience
getXp = progress_xp

isUnlocked :: Rune -> Progress -> Bool
isUnlocked rune progress =
  Set.member rune (progress_unlocks progress)

unlockNames :: Progress -> [Text]
unlockNames Progress {progress_unlocks} = getRuneName <$> Set.toList progress_unlocks

-- Partial progress for saving to JSON
data PartialProgress = PartialProgress
  { partialprogress_unlocks :: Set Rune,
    partialprogress_events :: Set Event
  }

instance ToJSON PartialProgress where
  toJSON p =
    object
      [ "unlocks" .= (getRuneName <$> Set.toList (partialprogress_unlocks p)),
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
data Event = EventTutorial | EventPuzzle
  deriving (Eq, Generic, Ord, Show)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Event
