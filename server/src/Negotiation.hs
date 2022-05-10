module Negotiation where

import Data.Text (Text)
import Util (breakAt)

data Request
  = RoomRequest Text
  | SystemMessageRequest Text

data Error
  = ConnectionLostError
  | UnknownError Text

parseRequest :: Text -> Either Error Request
parseRequest msg =
  case breakAt ":" msg of
    ("room", name) ->
      Right . RoomRequest $ name
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
