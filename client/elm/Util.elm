module Util exposing (apiLocation, curry, foldlWithPrev, httpErrorToString, interp, interp2D, interpFloat, maybeListToListMaybe, message, portProtocol, px, splitOnColon, splitOnComma, to3d, uncurry, zip)

import Http
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


baseLocation : Flags -> String
baseLocation { hostname, httpPort } =
    "https://" ++ hostname ++ portProtocol httpPort


apiLocation : Flags -> String
apiLocation flags =
    baseLocation flags ++ "/api"


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


splitOnComma : String -> ( String, String )
splitOnComma str =
    case Regex.fromString "," of
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


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "BadUrl " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus status ->
            "BadStatus: " ++ String.fromInt status

        Http.BadBody str ->
            "BadBody: " ++ str


foldlWithPrev : (Maybe a -> a -> b -> b) -> b -> List a -> b
foldlWithPrev =
    let
        f : Maybe a -> (Maybe a -> a -> b -> b) -> b -> List a -> b
        f prev step state list =
            case list of
                current :: remaining ->
                    let
                        newState =
                            step prev current state
                    in
                    f (Just current) step newState remaining

                _ ->
                    state
    in
    f Nothing


maybeListToListMaybe : Int -> Maybe (List a) -> List (Maybe a)
maybeListToListMaybe len mList =
    case mList of
        Just list ->
            let
                remainder =
                    List.repeat (max 0 (len - List.length list)) Nothing
            in
            List.map Just list ++ remainder

        Nothing ->
            List.repeat len Nothing
