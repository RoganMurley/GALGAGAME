module Negotiation where

import Data.Text (Text)
import Util (breakAt)


data RoomRequest =
    RoomRequest Text
  | ReconnectRequest
  | PlayReplayRequest Text


parseRoomReq :: Text -> Maybe RoomRequest
parseRoomReq msg =
  case breakAt ":" msg of
    ("reconnect", "") ->
      Just ReconnectRequest
    ("room", name) ->
      Just . RoomRequest $ name
    ("playReplay", replayId) ->
      Just . PlayReplayRequest $ replayId
    _ ->
      Nothing


parsePrefix :: Text -> Maybe Prefix
parsePrefix msg =
  case breakAt ":" msg of
    ("spectate", _)     -> Just PrefixSpec
    ("play", _)         -> Just PrefixPlay
    ("playComputer", _) -> Just PrefixCpu
    ("queue", _)        -> Just PrefixQueue
    _                   -> Nothing


data Prefix =
    PrefixSpec
  | PrefixPlay
  | PrefixCpu
  | PrefixQueue
