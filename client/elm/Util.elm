module Util exposing (..)

import Json.Decode as Json
import Main.Types exposing (Flags)
import Math.Vector3 exposing (Vec3)
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


websocketAddress : Flags -> String
websocketAddress { hostname, httpPort } =
    let
        portProtocol =
            if httpPort /= "" then
                ":" ++ httpPort
            else
                ""
    in
        "wss://" ++ hostname ++ portProtocol ++ "/game/"


authLocation : Flags -> String
authLocation { hostname, httpPort } =
    let
        portProtocol =
            if httpPort /= "" then
                ":" ++ httpPort
            else
                ""
    in
        "https://" ++ hostname ++ portProtocol ++ "/auth"


send : Flags -> String -> Cmd msg
send flags =
    WebSocket.send <| websocketAddress flags


splitOn : String -> String -> ( String, String )
splitOn sep str =
    case split (AtMost 1) (regex sep) str of
        [ x, xs ] ->
            ( x, xs )

        otherwise ->
            ( "", str )


unsafeForceDecode : Json.Decoder a -> String -> a
unsafeForceDecode decoder str =
    case Json.decodeString decoder str of
        Ok result ->
            result

        Err err ->
            Debug.crash err


maybeCons : Maybe a -> List a -> List a
maybeCons m xs =
    case m of
        Just x ->
            x :: xs

        Nothing ->
            xs


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


interp : Float -> Vec3 -> Vec3 -> Vec3
interp t start end =
    Math.Vector3.add start <|
        Math.Vector3.scale t <|
            Math.Vector3.sub end start


floatInterp : Float -> Float -> Float -> Float
floatInterp t start end =
    start + (t * (end - start))
