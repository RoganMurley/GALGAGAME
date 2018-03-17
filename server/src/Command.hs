module Command where

import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import Username (Username(..))
import Util (breakAt)


data Command =
    ChatCommand Username Text
  | PlayCommand Username
  | SpectateCommand Username
  | LeaveCommand Username
  | EndTurnCommand
  | PlayCardCommand Int
  | HoverCardCommand (Maybe Int)
  | RematchCommand
  | PlayReplayCommand
  | ConcedeCommand
  | SelectCharacterCommand Text
  | ErrorCommand Text
  deriving (Show)


parse :: Username -> Text -> Command
parse name msg =
  let
    (command, content) = breakAt ":" msg :: (Text, Text)
  in
    case command of
      "end" ->
        EndTurnCommand
      "play" ->
        case readMay . cs $ content of
          Just index ->
            PlayCardCommand index
          Nothing ->
            ErrorCommand (content <> " not a hand card index")
      "hover" ->
        case content of
          "null" ->
            HoverCardCommand Nothing
          _ ->
            case readMay . cs $ content of
              Just index ->
                HoverCardCommand (Just index)
              Nothing ->
                ErrorCommand (content <> " not a hand card index")
      "chat" ->
        ChatCommand name content
      "rematch" ->
        RematchCommand
      "watchReplay" ->
        PlayReplayCommand
      "concede" ->
        ConcedeCommand
      "selectCharacter" ->
        SelectCharacterCommand content
      _ ->
        ErrorCommand $ "Unknown Command " <> (cs $ show command)


toChat :: Command -> Text
toChat (SpectateCommand (Username name)) =
  "chat:" <> name <> " started spectating"
toChat (PlayCommand (Username name)) =
  "chat:" <> name <> " started playing"
toChat (LeaveCommand (Username name)) =
  "chat:" <> name <> " disconnected"
toChat (ChatCommand (Username name) message) =
  "chat:" <> name <> ": " <> message
toChat (ErrorCommand err) =
  "error:" <> err
toChat _ =
  "chat:" <> "Command cannot be processed to text :/"
