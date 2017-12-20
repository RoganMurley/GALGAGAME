module Util exposing (..)

import Regex exposing (HowMany(AtMost), regex, split)
import Task
import WebSocket


px : Int -> String
px number =
    toString number ++ "px"


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


safeTail : List a -> List a
safeTail l =
    case List.tail l of
        Just t ->
            t

        Nothing ->
            []


websocketAddress : String -> String
websocketAddress hostname =
    "wss://" ++ hostname ++ ":9160"


send : String -> String -> Cmd msg
send hostname =
    WebSocket.send <| websocketAddress hostname


splitOn : String -> String -> ( String, String )
splitOn sep str =
    case split (AtMost 1) (regex sep) str of
        [ x, xs ] ->
            ( x, xs )

        otherwise ->
            ( "", str )
