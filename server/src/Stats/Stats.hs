module Stats.Stats where

import Config (App, runBeam)
import Database.Beam ((==.), (<-.), all_, current_, filter_, runSelectReturningOne, runUpdate, select, update, val_)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Schema (GalgagameDb(..), galgagameDb)
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

load :: Text -> App Experience
load username = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ stats galgagameDb
  return $ fromMaybe 0 $ Stats.Schema.statsExperience <$> result

increase :: Text -> Experience -> App ()
increase username xp = do
  runBeam $ runUpdate $ update (stats galgagameDb)
    (\row -> [Stats.Schema.statsExperience row <-. current_ (Stats.Schema.statsExperience row) + val_ xp])
    (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId username))

data StatChange = StatChange
  { statChange_initialExperience :: Experience
  , statChange_finalExperience   :: Experience
  } deriving (Show, Eq)

instance ToJSON StatChange where
  toJSON (StatChange
    { statChange_initialExperience
    , statChange_finalExperience
    }) =
    object
      [ "initialExperience" .= statChange_initialExperience
      , "finalExperience"   .= statChange_finalExperience
      ]

statChange :: Experience -> Experience -> StatChange
statChange xp delta = StatChange
  { statChange_initialExperience = xp
  , statChange_finalExperience   = xp + delta
  }

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
