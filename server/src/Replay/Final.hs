module Replay.Final where

import qualified Auth.Schema
import Config (App, runBeam)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Int (Int64)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam (all_, default_, filter_, insert, insertExpressions, runSelectReturningOne, select, val_, (==.))
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import GameState (PlayState)
import Mirror (Mirror (..))
import Model (Model)
import Player (WhichPlayer (..))
import qualified Replay.Active as Active
import qualified Replay.Schema
import ResolveData (ResolveData (..))
import Schema (GalgagameDb (..), galgagameDb)

data Replay = Replay Active.Replay PlayState
  deriving (Show, Eq)

instance ToJSON Replay where
  toJSON (Replay (Active.Replay initial res (pa, _) (pb, _)) final) =
    object
      [ "list" .= res,
        "initial" .= initial,
        "final" .= final,
        "pa" .= pa,
        "pb" .= pb
      ]

instance Mirror Replay where
  mirror (Replay active final) =
    Replay (mirror active) (mirror final)

finalise :: Active.Replay -> PlayState -> Replay
finalise = Replay

getReplayUser :: WhichPlayer -> Replay -> Active.ReplayUser
getReplayUser which (Replay (Active.Replay _ _ pa pb) _) =
  case which of
    PlayerA ->
      pa
    PlayerB ->
      pb

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

getRes :: Replay -> [ResolveData]
getRes (Replay (Active.Replay _ res _ _) _) = res

getInitial :: Replay -> Model
getInitial (Replay (Active.Replay initial _ _ _) _) = initial
