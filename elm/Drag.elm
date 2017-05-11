module Drag exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse exposing (Position)
import Main.Messages exposing (..)
import Util exposing (px)


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Draggable a =
    { a
        | pos : Position
        , drag : Maybe Drag
    }


draggable : Draggable a -> List (Attribute Msg)
draggable model =
    let
        realPos : Position
        realPos =
            getPosition model
    in
        [ on "mousedown" (Json.map DragStart Mouse.position)
        , style
            [ ( "position", "absolute" )
            , ( "top", px realPos.y )
            , ( "left", px realPos.x )
            ]
        ]


getPosition : Draggable a -> Position
getPosition { pos, drag } =
    case drag of
        Nothing ->
            pos

        Just { start, current } ->
            Position
                (pos.x + current.x - start.x)
                (pos.y + current.y - start.y)


dragStart : Draggable a -> Position -> Draggable a
dragStart model pos =
    { model | drag = (Just (Drag pos pos)) }


dragAt : Draggable a -> Position -> Draggable a
dragAt model pos =
    { model | drag = (Maybe.map (\{ start } -> Drag start pos) model.drag) }


dragEnd : Draggable a -> Draggable a
dragEnd model =
    { model | pos = getPosition model, drag = Nothing }
