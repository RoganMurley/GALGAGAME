module Negotiation where

import Data.Text (Text)
import Util (breakAt)


parseRoomReq :: Text -> Maybe Text
parseRoomReq msg =
  case breakAt ":" msg of
    ("room", name) ->
      Just name
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
