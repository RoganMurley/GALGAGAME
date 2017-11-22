module Util exposing (..)

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


send : String -> String -> Cmd msg
send hostname =
    WebSocket.send <| "ws://" ++ hostname ++ ":9160"
