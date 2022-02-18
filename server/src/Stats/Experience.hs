module Stats.Experience where

import Data.Int (Int64)

type Experience = Int64

type Level = Int

levellingConstant :: Float
levellingConstant = 0.1

exponentConstant :: Float
exponentConstant = 1.8

levelFromExperience :: Experience -> Level
levelFromExperience xp = 1 + (floor $ levellingConstant * (fromIntegral xp) ** (1 / exponentConstant))

levelToExperience :: Level -> Experience
levelToExperience level = floor $ (fromIntegral (level - 1) / levellingConstant) ** exponentConstant
