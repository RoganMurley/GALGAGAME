module Replay.Final where

import Config (App, runBeam)
import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam ((==.), all_, default_, filter_, insertExpressions, runSelectReturningOne, select, val_)
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import GameState (PlayState)
import Mirror (Mirror(..))
import Player (WhichPlayer(..))
import Schema (RingOfWorldsDb(..), ringOfWorldsDb)

import qualified Auth.Schema

import qualified Replay.Active as Active
import qualified Replay.Schema


data Replay = Replay Active.Replay PlayState
  deriving (Show, Eq)


instance ToJSON Replay where
  toJSON (Replay (Active.Replay initial res pa pb) final) =
    object [
      "list"    .= res
    , "initial" .= initial
    , "final"   .= final
    , "pa"      .= pa
    , "pb"      .= pb
    ]


instance Mirror Replay where
  mirror (Replay active final) =
    Replay (mirror active) (mirror final)


finalise :: Active.Replay -> PlayState -> Replay
finalise = Replay


-- This is hacky, change replay usernames to be a sum type for CPU and Guest.
getUsername :: WhichPlayer -> Replay -> Maybe Text
getUsername which (Replay (Active.Replay _ _ ua ub) _) =
  let
    username :: Text
    username =
      case which of
        PlayerA ->
          ua
        PlayerB ->
          ub
  in
    case username of
      "cpu" ->
        Nothing
      "guest" ->
        Nothing
      _ ->
        Just username


save :: Replay -> App Int
save replay = do
  let playerA = getUsername PlayerA replay
  let playerB = getUsername PlayerB replay
  result <- runBeam $ runInsertReturningList (replays ringOfWorldsDb) $
    insertExpressions [
      Replay.Schema.Replay
        default_
        (val_ $ cs $ encode replay)
        (val_ $ Auth.Schema.UserId playerA)
        (val_ $ Auth.Schema.UserId playerB)
    ]
  return $ head $ Replay.Schema.replayId <$> result

load :: Int -> App (Maybe Text)
load replayId = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Replay.Schema.replayId row ==. val_ replayId) $
      all_ $ replays ringOfWorldsDb
  return $ Replay.Schema.replayReplay <$> result
