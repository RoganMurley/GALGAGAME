module Command where

import Data.Aeson (eitherDecode)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import DeckBuilding (CharacterChoice)
import Outcome (HoverState)
import Safe (readMay)
import Util (breakAt)

data Command
  = ChatCommand Text Text
  | PlayCommand Text
  | SpectateCommand Text
  | LeaveCommand Text
  | EndTurnCommand
  | PlayCardCommand Int
  | HoverCardCommand HoverState
  | RematchCommand
  | ConcedeCommand
  | SelectCharacterCommand CharacterChoice
  | EndEncounterCommand
  | GodModeCommand Text
  | HeartbeatCommand
  | ErrorCommand Text
  deriving (Show)

parse :: Text -> Text -> Command
parse name msg =
  let (command, content) = breakAt ":" msg :: (Text, Text)
   in case command of
        "end" ->
          EndTurnCommand
        "play" ->
          case readMay . cs $ content of
            Just index ->
              PlayCardCommand index
            Nothing ->
              ErrorCommand (content <> " not a hand card index")
        "hover" ->
          case eitherDecode $ cs content of
            Left err ->
              ErrorCommand $ cs err
            Right hover ->
              HoverCardCommand hover
        "chat" ->
          ChatCommand name content
        "rematch" ->
          RematchCommand
        "concede" ->
          ConcedeCommand
        "selectCharacter" ->
          case eitherDecode $ cs content of
            Left err ->
              ErrorCommand $ cs err
            Right choice ->
              SelectCharacterCommand choice
        "endEncounter" ->
          EndEncounterCommand
        "heartbeat" ->
          HeartbeatCommand
        "god" ->
          GodModeCommand content
        _ ->
          ErrorCommand $ "Unknown Command " <> (cs $ show command)

toChat :: Command -> Text
toChat = fromMaybe "error: unknown chat command" . toChatMaybe

toChatMaybe :: Command -> Maybe Text
toChatMaybe (SpectateCommand name) =
  Just $ "chat:" <> name <> " started spectating"
toChatMaybe (PlayCommand name) =
  Just $ "chat:" <> name <> " started playing"
toChatMaybe (LeaveCommand name) =
  Just $ "chat:" <> name <> " disconnected"
toChatMaybe (ChatCommand name message) =
  Just $ "chat:" <> name <> ": " <> message
toChatMaybe (ErrorCommand err) =
  Just $ "error:" <> err
toChatMaybe _ =
  Nothing
