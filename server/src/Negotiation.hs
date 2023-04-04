module Negotiation where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Read as Text.Read
import Scenario (CustomSettings (..), parseCustomSettings)
import Util (breakAt)

data Request
  = RoomRequest Text (Maybe CustomSettings)
  | ChallengeRequest Int64 Text
  | SystemMessageRequest Text

data Error
  = ConnectionLostError
  | UnknownError Text

parseTextToInt64 :: Text -> Either String Int64
parseTextToInt64 txt =
  case Text.Read.decimal txt of
    Left err ->
      Left err
    Right (n, _) ->
      Right n

parseRequest :: Text -> Either Error Request
parseRequest msg =
  case breakAt ":" msg of
    ("room", name) ->
      Right $ RoomRequest name Nothing
    ("createCustomRoom", body) ->
      case parseCustomSettings body of
        Left err ->
          Left . UnknownError $ err
        Right customSettings ->
          Right $ RoomRequest (customsettings_name customSettings) (Just customSettings)
    ("systemMessage", message) ->
      Right . SystemMessageRequest $ message
    ("heartbeat", _) ->
      Left ConnectionLostError
    ("challenge", message) ->
      let (opponentIdText, roomId) = breakAt "," message
       in case parseTextToInt64 opponentIdText of
            Right opponentId ->
              Right $ ChallengeRequest opponentId roomId
            Left _ ->
              Left . UnknownError $ "Cannot parse " <> opponentIdText <> " to int"
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
