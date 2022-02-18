module Negotiation where

import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)
import Util (breakAt)

data Request
  = RoomRequest Text
  | PlayReplayRequest Int
  | SystemMessageRequest Text

data Error
  = ConnectionLostError
  | UnknownError Text

parseRequest :: Text -> Either Error Request
parseRequest msg =
  case breakAt ":" msg of
    ("room", name) ->
      Right . RoomRequest $ name
    ("playReplay", replayIdText) ->
      case readMay $ cs replayIdText of
        Just replayId ->
          Right . PlayReplayRequest $ replayId
        Nothing ->
          Left . UnknownError $ "Unable to parse replay ID"
    ("systemMessage", message) ->
      Right . SystemMessageRequest $ message
    ("heartbeat", _) ->
      Left ConnectionLostError
    _ ->
      Left . UnknownError $ "Unknown request " <> msg

parsePrefix :: Text -> Maybe Prefix
parsePrefix msg =
  case breakAt ":" msg of
    ("spectate", _) -> Just PrefixSpec
    ("play", _) -> Just PrefixPlay
    ("playComputer", _) -> Just PrefixCpu
    ("queue", _) -> Just PrefixQueue
    _ -> Nothing

data Prefix
  = PrefixSpec
  | PrefixPlay
  | PrefixCpu
  | PrefixQueue
  deriving (Show)
