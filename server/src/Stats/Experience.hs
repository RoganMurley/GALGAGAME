module Stats.Experience where

import Data.Int (Int64)

type Experience = Int64

type Level = Int

levellingConstant :: Float
levellingConstant = 0.1

levelFromExperience :: Experience -> Level
levelFromExperience xp = 1 + (floor $ levellingConstant * sqrt (fromIntegral xp))

levelToExperience :: Level -> Experience
levelToExperience level = floor $ (fromIntegral (level - 1) / levellingConstant) ** 2
