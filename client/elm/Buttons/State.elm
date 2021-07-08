module Buttons.State exposing (empty, entity, fromList, get, getHoverText, hit, isCircular, isIcon, update)

import Buttons.Types exposing (Button, ButtonType(..), Buttons(..), TextButtonOption(..), TextButtonParams)
import Collision exposing (AABB, hitAABB)
import Dict
import List.Extra as List
import Math.Vector2 exposing (Vec2, vec2)
import Mouse exposing (MouseState(..))


entity :
    String
    -> { x : Float, y : Float, width : Float, height : Float, btn : ButtonType, disabled : Bool }
    -> Float
    -> MouseState
    -> Buttons
    -> ( String, Button )
entity key { x, y, width, height, btn, disabled } dt mouseState buttons =
    let
        previousHover : Float
        previousHover =
            get key buttons
                |> Maybe.map .hover
                |> Maybe.withDefault 0

        aabb : AABB
        aabb =
            { r1 = vec2 (x - abs width) (y - abs height)
            , r2 = vec2 (x + abs width) (y + abs height)
            }

        mMouse : Maybe Vec2
        mMouse =
            case mouseState of
                Mouse mouse ->
                    Just mouse

                _ ->
                    Nothing

        isHit : Bool
        isHit =
            case btn of
                TextButton params ->
                    if noHover params then
                        False

                    else
                        Maybe.map
                            (\mouse -> hitAABB aabb mouse)
                            mMouse
                            |> Maybe.withDefault False

                _ ->
                    Maybe.map (\mouse -> hitAABB aabb mouse) mMouse
                        |> Maybe.withDefault False

        hover : Float
        hover =
            if isHit then
                min (previousHover + dt) 300

            else
                max (previousHover - 4 * dt) 0
    in
    ( key
    , { x = x
      , y = y
      , hover = hover
      , width = width
      , height = height
      , disabled = disabled
      , btn = btn
      }
    )


empty : Buttons
empty =
    Buttons Dict.empty


fromList : List ( String, Button ) -> Buttons
fromList list =
    Buttons <| Dict.fromList list


get : String -> Buttons -> Maybe Button
get key (Buttons buttons) =
    Dict.get key buttons


hit : Buttons -> Vec2 -> Maybe ( String, Button )
hit (Buttons buttons) vec =
    let
        getAabb : Button -> AABB
        getAabb { x, y, width, height } =
            { r1 = vec2 (x - abs width) (y - abs height)
            , r2 = vec2 (x + abs width) (y + abs height)
            }
    in
    List.find
        (\( _, button ) -> hitAABB (getAabb button) vec)
        (Dict.toList buttons)


update : String -> (Button -> Button) -> Buttons -> Buttons
update key f (Buttons buttons) =
    Buttons <| Dict.update key (Maybe.map f) buttons


getHoverText : TextButtonParams -> String
getHoverText { text, options } =
    let
        reducer : TextButtonOption -> Maybe String -> Maybe String
        reducer option acc =
            case acc of
                Just hoverText ->
                    Just hoverText

                Nothing ->
                    case option of
                        HoverText hoverText ->
                            Just hoverText

                        _ ->
                            Nothing
    in
    List.foldl reducer Nothing options |> Maybe.withDefault text


isSomeBool : (TextButtonOption -> Bool) -> TextButtonParams -> Bool
isSomeBool f { options } =
    let
        reducer : TextButtonOption -> Bool -> Bool
        reducer option acc =
            if acc then
                True

            else
                f option
    in
    List.foldl reducer False options


isCircular : TextButtonParams -> Bool
isCircular =
    isSomeBool <|
        \option ->
            case option of
                Circular ->
                    True

                _ ->
                    False


isIcon : TextButtonParams -> Bool
isIcon =
    isSomeBool <|
        \option ->
            case option of
                IsIcon ->
                    True

                _ ->
                    False


noHover : TextButtonParams -> Bool
noHover =
    isSomeBool <|
        \option ->
            case option of
                NoHover ->
                    True

                _ ->
                    False
