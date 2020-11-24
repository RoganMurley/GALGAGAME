module Buttons.State exposing (empty, entity, fromList, get, getHoverText, hit, isCircular, update)

import Buttons.Types exposing (Button, ButtonType(..), Buttons(..), TextButtonOption(..), TextButtonParams)
import Collision exposing (AABB, hitAABB)
import Dict
import List.Extra as List
import Math.Vector2 exposing (Vec2, vec2)


entity :
    String
    -> { x : Float, y : Float, width : Float, height : Float, btn : ButtonType, disabled : Bool }
    -> Float
    -> Maybe Vec2
    -> Buttons
    -> ( String, Button )
entity key { x, y, width, height, btn, disabled } dt mMouse buttons =
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

        isHit : Bool
        isHit =
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


hit : Buttons -> Maybe ( String, Button )
hit (Buttons buttons) =
    List.find (\( _, { hover } ) -> hover > 0) (Dict.toList buttons)


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


isCircular : TextButtonParams -> Bool
isCircular { options } =
    let
        reducer : TextButtonOption -> Bool -> Bool
        reducer option acc =
            if acc then
                True

            else
                case option of
                    Circular ->
                        True

                    _ ->
                        False
    in
    List.foldl reducer False options
