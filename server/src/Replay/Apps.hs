module Replay.Apps where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (encode)
import Data.Int (Int64)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam (all_, default_, filter_, insert, insertExpressions, runSelectReturningOne, select, val_, (==.))
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Player (WhichPlayer (..))
import Replay.Final (Replay, getReplayUser)
import qualified Replay.Schema
import Schema (GalgagameDb (..), galgagameDb)

save :: Replay -> App Int64
save replay = do
  let (displayUsernamePa, playerA) = getReplayUser PlayerA replay
  let (displayUsernamePb, playerB) = getReplayUser PlayerB replay
  result <-
    runBeam $
      runInsertReturningList $
        insert (replays galgagameDb) $
          insertExpressions
            [ Replay.Schema.Replay
                default_
                default_
                (val_ $ cs $ encode replay)
                (val_ $ Auth.Schema.UserId playerA)
                (val_ $ Auth.Schema.UserId playerB)
                (val_ $ Just displayUsernamePa)
                (val_ $ Just displayUsernamePb)
            ]
  return $ head $ Replay.Schema.replayId <$> result

load :: Int64 -> App (Maybe Text)
load replayId = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Replay.Schema.replayId row ==. val_ replayId) $
            all_ $ replays galgagameDb
  return $ Replay.Schema.replayReplay <$> result