module Replay.Apps where

import Auth.Schema qualified
import Config (App, runBeam)
import Data.Aeson (encode)
import Data.Int (Int64)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam (all_, default_, filter_, insert, insertExpressions, runSelectReturningOne, select, val_, (==.))
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Player (WhichPlayer (..))
import Replay.Final (Replay, getReplayUser)
import Replay.Schema qualified
import Safe (headMay)
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
  case headMay result of
    Just r ->
      return $ Replay.Schema.replayId r
    Nothing ->
      error "Failed to save replay"

load :: Int64 -> App (Maybe Text)
load replayId = do
  result <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Replay.Schema.replayId row ==. val_ replayId) $
            all_ $
              replays galgagameDb
  return $ Replay.Schema.replayReplay <$> result