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
import Player (WhichPlayer (..))
import qualified Replay.Active as Active
import qualified Replay.Schema
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

getUsername :: WhichPlayer -> Replay -> Maybe Text
getUsername which (Replay (Active.Replay _ _ (_, pa) (_, pb)) _) =
  let textUsername =
        case which of
          PlayerA ->
            pa
          PlayerB ->
            pb
   in case textUsername of
        "" ->
          Nothing
        s ->
          Just s

save :: Replay -> App Int64
save replay = do
  let playerA = getUsername PlayerA replay
  let playerB = getUsername PlayerB replay
  result <-
    runBeam $
      runInsertReturningList $
        insert (replays galgagameDb) $
          insertExpressions
            [ Replay.Schema.Replay
                default_
                (val_ $ cs $ encode replay)
                (val_ $ Auth.Schema.UserId playerA)
                (val_ $ Auth.Schema.UserId playerB)
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
