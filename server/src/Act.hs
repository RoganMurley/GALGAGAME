module Act where

import Config (App)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Data.Aeson (encode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GameCommand (GameCommand(..), update)
import GameState (GameState(..), PlayState)
import Mirror (mirror)
import Model (Model)
import Player (WhichPlayer(..), other)
import ResolveData (ResolveData(..))
import Scenario (Scenario(..))
import Text.Printf (printf)
import Util (Err, modReturnTVar)


import qualified Command
import Command (Command(..))

import qualified Client
import Client (Client(..))

import qualified Log

import qualified Outcome
import Outcome (Outcome)

import qualified Replay.Final

import qualified Room
import Room (Room)

import qualified Stats.Stats as Stats


roomUpdate :: GameCommand -> WhichPlayer -> UTCTime -> TVar Room -> STM (Room, Either Err [Outcome])
roomUpdate cmd which time roomVar =
  modReturnTVar roomVar $ \room ->
    case updateRoom room of
      Left err ->
        (room, (room, Left err))
      Right (Nothing, o) ->
        (room, (room, Right o))
      Right (Just r, o) ->
        (r, (r, Right o))
  where
    updateRoom :: Room -> Either Err (Maybe Room, [Outcome])
    updateRoom room =
      case update cmd which (Room.getState room) (Room.getScenario room) (Room.getUsers room) time of
        Left err ->
          Left err
        Right (newState, outcomes) ->
          Right ((Room.setState room) <$> newState, outcomes)


actPlay :: Command -> WhichPlayer -> TVar Room -> Text -> App ()
actPlay cmd which roomVar username = do
  Log.info $ printf "<%s>: %s" username (show cmd)
  case trans cmd of
    Just command -> do
      time <- liftIO getCurrentTime
      (room, updated) <- liftIO $ atomically $ roomUpdate command which time roomVar
      case updated of
        Left err -> do
          Log.warning $ printf "Command error: %s" (show err)
          Room.sendToPlayer which (Command.toChat (ErrorCommand err)) room
        Right outcomes ->
          forM_ outcomes (actOutcome room)
    Nothing ->
      actSpec cmd roomVar
  where
    trans :: Command -> Maybe GameCommand
    trans EndTurnCommand             = Just EndTurn
    trans (PlayCardCommand index)    = Just (PlayCard index)
    trans (HoverCardCommand hover)   = Just (HoverCard hover)
    trans RematchCommand             = Just Rematch
    trans ConcedeCommand             = Just Concede
    trans (ChatCommand name content) = Just (Chat name content)
    trans (SelectCharacterCommand c) = Just (SelectCharacter c)
    trans (GodModeCommand msg)       = Just (God msg)
    trans HeartbeatCommand           = Just Heartbeat
    trans _                          = Nothing


actSpec :: Command -> TVar Room -> App ()
actSpec cmd roomVar = do
  room <- liftIO . atomically $ readTVar roomVar
  case Command.toChatMaybe cmd of
    Just msg ->
      Room.broadcast msg room
    Nothing ->
      return ()


syncClient :: Client -> WhichPlayer -> GameState -> App ()
syncClient client which game =
  case which of
    PlayerA ->
      Client.send (("sync:" <>) . cs . encode $ game) client
    PlayerB ->
      Client.send (("sync:" <>) . cs . encode . mirror $ game) client


syncRoomClients :: Room -> App ()
syncRoomClients room = do
  Room.sendToPlayer PlayerA syncMsgPa room
  Room.sendToPlayer PlayerB syncMsgPb room
  Room.sendToSpecs syncMsgPa room
  where
    game = Room.getState room :: GameState
    syncMsgPa = ("sync:" <>) . cs . encode $ game :: Text
    syncMsgPb = ("sync:" <>) . cs . encode . mirror $ game :: Text


syncPlayersRoom :: Room -> App ()
syncPlayersRoom room = do
  Room.sendExcluding PlayerB syncMsgPa room
  Room.sendToPlayer  PlayerB syncMsgPb room
  where
    syncMsgPa :: Text
    syncMsgPa = ("syncPlayers:" <>) . cs . encode $ Room.players room
    syncMsgPb :: Text
    syncMsgPb = ("syncPlayers:" <>) . cs . encode . mirror $ Room.players room


resolveRoomClients :: [ResolveData] -> Model -> PlayState -> Maybe WhichPlayer -> Room -> App ()
resolveRoomClients res initial final exclude room = do
  when (exclude /= Just PlayerA) $ Room.sendToPlayer PlayerA msgPa room
  when (exclude /= Just PlayerB) $ Room.sendToPlayer PlayerB msgPb room
  Room.sendToSpecs msgPa room
  where
    msgPa = ("res:" <>) . cs . encode $ outcome :: Text
    msgPb = ("res:" <>) . cs . encode $ mirrorOutcome :: Text
    outcome :: Outcome.Encodable
    outcome = Outcome.Resolve res initial final exclude
    mirrorOutcome :: Outcome.Encodable
    mirrorOutcome = Outcome.Resolve (mirror <$> res) (mirror initial) (mirror final) (other <$> exclude)


handleExperience :: WhichPlayer -> Maybe WhichPlayer -> Room -> App ()
handleExperience which winner room = do
  -- Change this to be a transaction!
  -- Save usernames all game.
  let scenario = Room.getScenario room
  let mUsername = Room.getPlayerClient which room >>= Client.queryUsername :: Maybe Text
  case mUsername of
    Just username -> do
      initialXp <- Stats.load username
      let xpDelta = if Just which == winner then scenario_xpWin scenario else scenario_xpLoss scenario
      Stats.increase username xpDelta
      let statChange = Stats.statChange initialXp xpDelta
      Log.info $ printf "Xp change for %s: %s" username (show statChange)
      Room.sendToPlayer which (("xp:" <>) . cs . encode $ statChange) room
    Nothing -> do
      Log.info "There's nobody here to gain that sweet xp :("
      return ()

actOutcome :: Room -> Outcome -> App ()
actOutcome room Outcome.Sync =
  syncRoomClients room
actOutcome room (Outcome.Encodable o@(Outcome.Hover which _ rawDmg)) = do
  Room.sendExcluding which (("hover:" <>) . cs . encode $ o) room
  let dmg = if which == PlayerA then rawDmg else mirror rawDmg
  Room.sendToPlayer which (("damage:" <>) . cs . encode $ dmg) room
actOutcome room (Outcome.Encodable (Outcome.Chat username msg)) =
  Room.broadcast ("chat:" <> username <> ": " <> msg) room
actOutcome room (Outcome.Encodable (Outcome.Resolve models initial final exclude)) =
  resolveRoomClients models initial final exclude room
actOutcome room (Outcome.Encodable (Outcome.Heartbeat timeLeft)) =
  Room.broadcast ("timeLeft:" <> cs (show (1000 * realToFrac timeLeft :: Float))) room
actOutcome room (Outcome.SaveReplay replay) = do
  Log.info "Saving replay..."
  replayId <- Replay.Final.save replay
  Room.broadcast ("replaySaved:" <> (cs . show $ replayId)) room
actOutcome room (Outcome.HandleExperience winner) = do
  handleExperience PlayerA winner room
  handleExperience PlayerB winner room
