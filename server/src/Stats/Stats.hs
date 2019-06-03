module Stats.Stats where

import Data.Int (Int64)

type Experience = Int64
type Level = Int

levellingConstant :: Float
levellingConstant = 0.2

levelFromExperience :: Experience -> Level
levelFromExperience xp = floor $ levellingConstant * sqrt (fromIntegral xp)

levelToExperience :: Level -> Experience
levelToExperience level = floor $ (fromIntegral level / levellingConstant) ** 2

nextLevelAt :: Experience -> Experience
nextLevelAt xp = levelToExperience nextLevel
  where
    currentLevel = levelFromExperience xp :: Level
    nextLevel = currentLevel + 1 :: Level
