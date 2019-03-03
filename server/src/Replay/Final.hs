module Replay.Final where

import Config (App, getReplayConn)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Text (Text)
import GameState (PlayState)
import Mirror (Mirror(..))
import Safe (headMay)
import System.Log.Logger (debugM)
import Text.Printf (printf)

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Replay.Active as Active


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
  conn <- getReplayConn
  result <- liftIO $ Postgres.query conn "INSERT INTO replays (replay) VALUES (?) RETURNING id" (Postgres.Only $ encode replay)
  liftIO $ debugM "app" $ printf "Replay result: %s" $ show result
  return $ Postgres.fromOnly $ head result

load :: Int -> App (Maybe Text)
load replayId = do
  conn <- getReplayConn
  result <- liftIO (Postgres.query conn "SELECT replay FROM replays WHERE id=? LIMIT 1" (Postgres.Only replayId) :: IO [Postgres.Only Text])
  return $ Postgres.fromOnly <$> headMay result
