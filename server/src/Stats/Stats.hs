module Stats.Stats where

import qualified Auth.Schema
import Config (App, runBeam)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), decode, encode, object, (.=))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Database.Beam (all_, filter_, insertValues, primaryKey, runInsert, runSelectReturningOne, runUpdate, select, update, val_, (<-.), (==.))
import qualified Database.Beam.Postgres.Full as Postgres
import DeckBuilding (Rune (..), mainRunes)
import Quest (Quest)
import qualified Quest
import Replay.Final (Replay, getInitial, getRes)
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Experience (Experience, levelToExperience)
import Stats.Progress (Progress (..), fromPartial, initialProgress)
import qualified Stats.Schema
import User.User (User (..))
import Util (Gen, getGen)

load :: User -> App Progress
load u@(User user _) = do
  time <- liftIO getCurrentTime
  gen <- liftIO getGen
  let userId = Auth.Schema.userId user :: Int64
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId)) $
            all_ $ stats galgagameDb
  let progress =
        result
          >>= ( \r ->
                  let xp = Stats.Schema.statsExperience r
                      partial =
                        Stats.Schema.statsProgress r
                          >>= return . cs
                          >>= decode
                   in fromPartial <$> partial <*> Just xp
              )
  finalProgress <- refreshQuests gen time $ fromMaybe initialProgress progress
  updateProgress u finalProgress
  return finalProgress
load (GuestUser cid _) = loadByCid $ Just cid
load _ = return initialProgress

loadByCid :: Maybe Text -> App Progress
loadByCid (Just cid) = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Stats.Schema.statsguestCid row ==. val_ cid) $
            all_ $ statsguest galgagameDb
  let progress =
        result
          >>= ( \r ->
                  let xp = Stats.Schema.statsguestExperience r
                      partial =
                        Stats.Schema.statsguestProgress r
                          >>= return . cs
                          >>= decode
                   in fromPartial <$> partial <*> Just xp
              )
  return $ fromMaybe initialProgress progress
loadByCid Nothing = return initialProgress

refreshQuests :: Gen -> UTCTime -> Progress -> App Progress
refreshQuests gen currentTime progress =
  if progress_xp progress < levelToExperience 5 -- Only at level 5 do quests become a thing.
    then return progress
    else
      ( case progress_questupdate progress of
          Nothing -> do
            return $ newQuests gen currentTime progress
          Just updatedAt ->
            let delta = diffUTCTime currentTime updatedAt :: NominalDiffTime
                hour = 60 * minute
                minute = 60
             in if delta > hour then return $ newQuests gen currentTime progress else return progress
      )

updateProgress :: User -> Progress -> App ()
updateProgress (User user _) progress = do
  let userId = Auth.Schema.userId user :: Int64
  let xp = progress_xp progress :: Experience
  runBeam $
    runUpdate $
      update
        (stats galgagameDb)
        ( \row ->
            mconcat
              [ Stats.Schema.statsExperience row <-. val_ xp,
                Stats.Schema.statsProgress row <-. val_ (Just . cs $ encode progress)
              ]
        )
        (\row -> Stats.Schema.statsUser row ==. val_ (Auth.Schema.UserId userId))
updateProgress (GuestUser cid _) progress = do
  let xp = progress_xp progress :: Experience
  let encodedProgress = Just . cs $ encode progress
  let val = Stats.Schema.Statsguest cid xp encodedProgress
  runBeam $
    runInsert $
      Postgres.insertOnConflict
        (statsguest galgagameDb)
        (insertValues [val])
        (Postgres.conflictingFields primaryKey)
        ( Postgres.onConflictUpdateSet
            ( \row _ ->
                mconcat
                  [ Stats.Schema.statsguestExperience row <-. val_ xp,
                    Stats.Schema.statsguestProgress row <-. val_ encodedProgress
                  ]
            )
        )
updateProgress _ _ = return ()

data StatChange = StatChange
  { statChange_initialExperience :: Experience,
    statChange_finalExperience :: Experience,
    statChange_newUnlocks :: Set Rune,
    statChange_questChange :: Set Quest
  }
  deriving (Show, Eq)

instance ToJSON StatChange where
  toJSON
    StatChange
      { statChange_initialExperience,
        statChange_finalExperience,
        statChange_newUnlocks,
        statChange_questChange
      } =
      object
        [ "initialExperience" .= statChange_initialExperience,
          "finalExperience" .= statChange_finalExperience,
          "unlocks" .= statChange_newUnlocks,
          "quests" .= statChange_questChange
        ]

statChange :: Progress -> Progress -> StatChange
statChange initial final =
  let -- Unlocks
      initialUnlocks = progress_unlocks initial
      finalUnlocks = progress_unlocks final
      newUnlocks = Set.difference finalUnlocks initialUnlocks
      -- Quests
      initialQuests = progress_quests initial
      finalQuests = progress_quests final
      questChanges = Set.difference initialQuests finalQuests
   in StatChange
        { statChange_initialExperience = progress_xp initial,
          statChange_finalExperience = progress_xp final,
          statChange_newUnlocks = newUnlocks,
          statChange_questChange = questChanges
        }

hydrateUnlocks :: Progress -> Progress
hydrateUnlocks progress =
  progress {progress_unlocks = Set.union unlocks newUnlocks}
  where
    unlocks = progress_unlocks progress
    xp = progress_xp progress
    newUnlocks =
      Set.fromList $
        filter
          (\rune -> xp >= rune_xp rune)
          mainRunes

updateQuests :: Replay -> [Text] -> Progress -> Progress
updateQuests replay tags progress = progress {progress_quests = finalQuests, progress_xp = xp + xpDelta}
  where
    quests = progress_quests progress
    xp = progress_xp progress
    res = getRes replay
    initial = getInitial replay
    finalQuests = Quest.test tags quests initial res
    questChanges = Set.difference quests finalQuests
    xpDelta = Set.fold (\q x -> x + Quest.quest_xp q) 0 questChanges

isChange :: StatChange -> Bool
isChange StatChange {statChange_initialExperience, statChange_finalExperience, statChange_newUnlocks, statChange_questChange} =
  (statChange_initialExperience /= statChange_finalExperience)
    || not (Set.null statChange_newUnlocks)
    || not (Set.null statChange_questChange)

newQuests :: Gen -> UTCTime -> Progress -> Progress
newQuests gen updatedAt progress =
  progress
    { progress_questupdate = Just updatedAt,
      progress_quests = Quest.choose gen $ progress_unlocks progress
    }
