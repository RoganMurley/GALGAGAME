module Stats.Stats where

import Config (App, runBeam)
import Database.Beam ((==.), all_, filter_, runSelectReturningOne, select, val_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Schema (RingOfWorldsDb(..), ringOfWorldsDb)
import qualified Stats.Schema
import qualified Auth.Schema

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

load :: Text -> App Experience
load username = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ stats ringOfWorldsDb
  return $ fromMaybe 0 $ Stats.Schema.statsExperience <$> result
