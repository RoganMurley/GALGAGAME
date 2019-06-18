module Util exposing (authLocation, interp, interp2D, interpFloat, message, portProtocol, px, splitOnColon, to3d, zip)

import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Regex
import Task


px : Float -> String
px number =
    String.fromFloat number ++ "px"


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


portProtocol : String -> String
portProtocol httpPort =
    case httpPort of
        "" ->
            ""

        _ ->
            ":" ++ httpPort


authLocation : Flags -> String
authLocation { hostname, httpPort } =
    "https://" ++ hostname ++ portProtocol httpPort ++ "/auth"


splitOnColon : String -> ( String, String )
splitOnColon str =
    case Regex.fromString ":" of
        Just regex ->
            case Regex.splitAtMost 1 regex str of
                [ x, xs ] ->
                    ( x, xs )

                _ ->
                    ( "", str )

        Nothing ->
            ( "", str )


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (\a b -> ( a, b ))


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


interpFloat : Float -> Float -> Float -> Float
interpFloat t start end =
    start + (t * (end - start))


to3d : Vec2 -> Vec3
to3d pos =
    vec3 (Math.Vector2.getX pos) (Math.Vector2.getY pos) 0
