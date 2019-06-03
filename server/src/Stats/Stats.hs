module Stats.Stats where

import Data.Int (Int64)

type Experience = Int64
type Level = Int

levelFromExperience :: Experience -> Level
levelFromExperience xp = floor $ 0.2 * sqrt xp
