module Stats.Stats where

import Config (App, runBeam)
import Database.Beam ((==.), (<-.), all_, current_, filter_, runSelectReturningOne, runUpdate, select, update, val_)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Schema (RingOfWorldsDb(..), ringOfWorldsDb)
import qualified Stats.Schema
import qualified Auth.Schema

type Experience = Int64
type Level = Int

levellingConstant :: Float
levellingConstant = 0.1

levelFromExperience :: Experience -> Level
levelFromExperience xp = 1 + (floor $ levellingConstant * sqrt (fromIntegral xp))

levelToExperience :: Level -> Experience
levelToExperience level = floor $ (fromIntegral (level - 1) / levellingConstant) ** 2

nextLevelAt :: Experience -> Experience
nextLevelAt xp = levelToExperience nextLevel
  where
    currentLevel = levelFromExperience xp :: Level
    nextLevel = currentLevel + 1 :: Level

experienceThisLevel :: Experience -> Experience
experienceThisLevel xp = xp - (levelToExperience . levelFromExperience $ xp)

load :: Text -> App Experience
load username = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ stats ringOfWorldsDb
  return $ fromMaybe 0 $ Stats.Schema.statsExperience <$> result

increase :: Text -> Experience -> App ()
increase username xp = do
  runBeam $ runUpdate $ update (stats ringOfWorldsDb)
    (\row -> [Stats.Schema.statsExperience row <-. current_ (Stats.Schema.statsExperience row) + val_ xp])
    (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username))

data StatChange = StatChange
  { statChange_initialLevel      :: Level
  , statChange_initialExperience :: Experience
  , statChange_finalLevel        :: Level
  , statChange_finalExperience   :: Experience
  , statChange_nextLevelAt       :: Experience
  } deriving (Show, Eq)

instance ToJSON StatChange where
  toJSON (StatChange
    { statChange_initialLevel
    , statChange_initialExperience
    , statChange_finalLevel
    , statChange_finalExperience
    , statChange_nextLevelAt
    }) =
    object
      [ "initialLevel"      .= statChange_initialLevel
      , "initialExperience" .= statChange_initialExperience
      , "finalLevel"        .= statChange_finalLevel
      , "finalExperience"   .= statChange_finalExperience
      , "nextLevelAt"       .= statChange_nextLevelAt
      ]

statChange :: Experience -> Experience -> StatChange
statChange xp delta = StatChange
  { statChange_initialLevel      = levelFromExperience xp
  , statChange_initialExperience = xp
  , statChange_finalLevel        = levelFromExperience finalXp
  , statChange_finalExperience   = finalXp
  , statChange_nextLevelAt       = nextLevelAt finalXp
  }
  where
    finalXp = xp + delta :: Experience

legalCharacters :: Level -> [String]
legalCharacters 0     = []
legalCharacters level =
  case characterUnlockedAtLevel level of
    Just character ->
      character : otherCharacters
    Nothing ->
      otherCharacters
  where
    otherCharacters :: [String]
    otherCharacters = legalCharacters (level - 1)
    characterUnlockedAtLevel :: Level -> Maybe String
    characterUnlockedAtLevel 1 = Just "Catherine"
    characterUnlockedAtLevel 2 = Just "Ophanim"
    characterUnlockedAtLevel _ = Nothing
