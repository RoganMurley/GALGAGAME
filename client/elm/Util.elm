module Util exposing (..)

import Task


-- Convert to pixel css representation.


px : Int -> String
px number =
    toString number ++ "px"



-- Turn a message into a command.


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)



-- Unsafe fromJust.


fromJust : Maybe a -> a
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.crash "error: fromJust Nothing"



-- A safe tail.


safeTail : List a -> List a
safeTail l =
    case List.tail l of
        Just t ->
            t

        Nothing ->
            []



--
