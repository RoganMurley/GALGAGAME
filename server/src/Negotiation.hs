module Negotiation where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)
import Util (breakAt)


data Request =
    RoomRequest Text
  | PlayReplayRequest Int
  | SystemMessageRequest Text
  | WorldRequest


parseRequest :: Text -> Maybe Request
parseRequest msg =
  case breakAt ":" msg of
    ("room", name) ->
      Just . RoomRequest $ name
    ("playReplay", replayIdText) ->
      case readMay $ cs replayIdText of
        Just replayId ->
          Just . PlayReplayRequest $ replayId
        Nothing ->
          Nothing
    ("systemMessage", message) ->
      Just . SystemMessageRequest $ message
    ("world", _) ->
      Just WorldRequest
    _ ->
      Nothing


parsePrefix :: Text -> Maybe Prefix
parsePrefix msg =
  case breakAt ":" msg of
    ("spectate", _)     -> Just PrefixSpec
    ("play", _)         -> Just PrefixPlay
    ("playComputer", _) -> Just PrefixCpu
    ("playTutorial", _) -> Just PrefixTutorial
    ("playDaily", _)    -> Just PrefixDaily
    ("queue", _)        -> Just PrefixQueue
    _                   -> Nothing


data Prefix =
    PrefixSpec
  | PrefixPlay
  | PrefixCpu
  | PrefixTutorial
  | PrefixDaily
  | PrefixQueue
  deriving Show
