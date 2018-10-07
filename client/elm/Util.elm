module Util exposing (..)

import Json.Decode as Json
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Regex exposing (HowMany(AtMost), regex, split)
import Task
import WebSocket


px : n -> String
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


portProtocol : String -> String
portProtocol httpPort =
    case httpPort of
        "" ->
            ""

        _ ->
            ":" ++ httpPort


websocketAddress : Flags -> String
websocketAddress { hostname, httpPort } =
    "wss://" ++ hostname ++ portProtocol httpPort ++ "/game/"


authLocation : Flags -> String
authLocation { hostname, httpPort } =
    "https://" ++ hostname ++ portProtocol httpPort ++ "/auth"


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


interp2D : Float -> Vec2 -> Vec2 -> Vec2
interp2D t start end =
    Math.Vector2.add start <|
        Math.Vector2.scale t <|
            Math.Vector2.sub end start


floatInterp : Float -> Float -> Float -> Float
floatInterp t start end =
    start + (t * (end - start))


to3d : Vec2 -> Vec3
to3d pos =
    vec3 (Math.Vector2.getX pos) (Math.Vector2.getY pos) 0
