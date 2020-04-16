module Command where

import Data.Aeson (eitherDecode)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import DeckBuilding (CharacterChoice)
import Outcome (HoverState)
import Safe (readMay)

import Util (breakAt)


data Command =
    ChatCommand Text Text
  | PlayCommand Text
  | SpectateCommand Text
  | LeaveCommand Text
  | EndTurnCommand
  | PlayCardCommand Int
  | HoverCardCommand HoverState
  | RematchCommand
  | ConcedeCommand
  | SelectCharacterCommand CharacterChoice
  | GodModeCommand Text
  | ErrorCommand Text
  deriving (Show)


parse :: Text -> Text -> Command
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
      "god" ->
        GodModeCommand content
      _ ->
        ErrorCommand $ "Unknown Command " <> (cs $ show command)


toChat :: Command -> Text
toChat (SpectateCommand name) =
  "chat:" <> name <> " started spectating"
toChat (PlayCommand name) =
  "chat:" <> name <> " started playing"
toChat (LeaveCommand name) =
  "chat:" <> name <> " disconnected"
toChat (ChatCommand name message) =
  "chat:" <> name <> ": " <> message
toChat (ErrorCommand err) =
  "error:" <> err
toChat _ =
  "chat:" <> "Command cannot be processed to text :/"
