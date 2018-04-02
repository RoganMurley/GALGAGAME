module Act where

import CardAnim (CardAnim)
import Config (App)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Data.Aeson (encode)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GameCommand (GameCommand(..), update)
import GameState (GameState(..), PlayState)
import Mirror (mirror)
import Model (Model)
import ModelDiff (ModelDiff)
import Player (WhichPlayer(..))
import StackCard (StackCard)
import System.Log.Logger (infoM, warningM)
import Text.Printf (printf)
import Username (Username(Username))
import Util (Err, modReturnTVar)


import qualified Command
import Command (Command(..))

import qualified Client
import Client (Client(..))

import qualified Outcome
import Outcome (Outcome)

import qualified Replay.Final

import qualified Room
import Room (Room)


roomUpdate :: GameCommand -> WhichPlayer -> TVar Room -> STM (Room, Either Err [Outcome])
roomUpdate cmd which roomVar =
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
      case update cmd which (Room.getState room) (Room.getUsernames room) of
        Left err ->
          Left err
        Right (newState, outcomes) ->
          Right ((Room.setState room) <$> newState, outcomes)


actPlay :: Command -> WhichPlayer -> TVar Room -> App ()
actPlay cmd which roomVar = do
  liftIO $ infoM "app" $ printf "Command: %s" (show cmd)
  case trans cmd of
    Just command -> do
      (room, updated) <- liftIO $ atomically $ roomUpdate command which roomVar
      case updated of
        Left err -> do
          liftIO $ warningM "app" $ printf "Command error: %s" (show err)
          Room.sendToPlayer which (Command.toChat (ErrorCommand err)) room
        Right outcomes ->
          forM_ outcomes (actOutcome room)
    Nothing ->
      actSpec cmd roomVar
  where
    trans :: Command -> Maybe GameCommand
    trans EndTurnCommand             = Just EndTurn
    trans (PlayCardCommand index)    = Just (PlayCard index)
    trans (HoverCardCommand index)   = Just (HoverCard index)
    trans RematchCommand             = Just Rematch
    trans ConcedeCommand             = Just Concede
    trans (ChatCommand name content) = Just (Chat name content)
    trans (SelectCharacterCommand n) = Just (SelectCharacter n)
    trans _                          = Nothing


actSpec :: Command -> TVar Room -> App ()
actSpec cmd roomVar = do
  room <- liftIO . atomically $ readTVar roomVar
  Room.broadcast (Command.toChat cmd) room


syncClient :: Client -> GameState -> App ()
syncClient client game =
  Client.send (("sync:" <>) . cs . encode $ game) client


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
  Room.sendExcluding PlayerB (syncMsg True) room
  Room.sendToPlayer PlayerB (syncMsg False) room
  where
    syncMsg :: Bool -> Text
    syncMsg rev =
      "syncPlayers:" <>
        (cs . encode . (if rev then mirror else id) $ Room.connected room)


resolveRoomClients :: ([(ModelDiff, Maybe CardAnim, Maybe StackCard)], Model, PlayState) -> Room -> App ()
resolveRoomClients (resList, initial, final) room = do
  Room.sendToPlayer PlayerA msgPa room
  Room.sendToPlayer PlayerB msgPb room
  Room.sendToSpecs msgPa room
  where
    msgPa = ("res:" <>) . cs . encode $ outcome :: Text
    msgPb = ("res:" <>) . cs . encode $ mirrorOutcome :: Text
    outcome :: Outcome.Encodable
    outcome =
      Outcome.Resolve resList initial final
    mirrorOutcome :: Outcome.Encodable
    mirrorOutcome =
      Outcome.Resolve
        ((\(x, y, z) -> (mirror x, mirror y, mirror z)) <$> resList)
        (mirror initial)
        (mirror final)


actOutcome :: Room -> Outcome -> App ()
actOutcome room Outcome.Sync =
  syncRoomClients room
actOutcome room (Outcome.Encodable o@(Outcome.Hover which _)) =
  Room.sendExcluding which (("hover:" <>) . cs . encode $ o) room
actOutcome room (Outcome.Encodable (Outcome.Chat (Username username) msg)) =
  Room.broadcast ("chat:" <> username <> ": " <> msg) room
actOutcome room (Outcome.Encodable (Outcome.Resolve models initial final)) =
  resolveRoomClients (models, initial, final) room
actOutcome room (Outcome.SaveReplay replay) = do
  liftIO $ infoM "app" "Saving replay..."
  result <- Replay.Final.save replay
  case result of
    Just replayId ->
      Room.broadcast ("replaySaved:" <> replayId) room
    Nothing ->
      liftIO $ warningM "app" "Failed to save replay"
