module Replay.Final where

import Config (App, runBeam)
import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam ((==.), all_, default_, filter_, insertExpressions, runSelectReturningOne, select, val_)
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import GameState (PlayState)
import Mirror (Mirror(..))
import Schema (RingOfWorldsDb(..), ringOfWorldsDb)

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


save :: Replay -> App Int
save replay = do
  result <- runBeam $ runInsertReturningList (replays ringOfWorldsDb) $
    insertExpressions [ Replay.Schema.Replay default_ (val_ $ cs $ encode replay) ]
  return $ head $ Replay.Schema.replayId <$> result


load :: Int -> App (Maybe Text)
load replayId = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Replay.Schema.replayId row ==. val_ replayId) $
      all_ $ replays ringOfWorldsDb
  return $ Replay.Schema.replayReplay <$> result
