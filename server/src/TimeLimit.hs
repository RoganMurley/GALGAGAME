module TimeLimit where

import Config (App)
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Monad (forever, mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import GameState (GameState(..), PlayState(..))
import Model (Model(..))
import Scenario (Scenario(..))

import qualified Log

import qualified Room
import Room (Room(..))


asyncTracker :: TVar Room -> App ()
asyncTracker roomVar = do
    _ <- fork $ (runMaybeT . forever $ loop) >> return ()
    return ()
  where
    loop :: MaybeT App ()
    loop = do
      liftIO $ threadDelay 1000
      room <- liftIO $ readTVarIO roomVar
      case Room.getState room of
        Started (Playing model _) -> do
          currentTime <- liftIO getCurrentTime
          let lastInteraction = fromMaybe currentTime (model_lastInteraction model)
          let delta = diffUTCTime currentTime lastInteraction
          let timeLimit = fromIntegral $ scenario_timeLimit (Room.getScenario room) :: NominalDiffTime
          if delta > timeLimit then
            lift $ Log.info $ printf "Time limit exceeded! %s" (show delta)
              else
                return ()
        _ ->
          return ()
      -- Break out if the room's empty.
      when (Room.empty room) mzero
