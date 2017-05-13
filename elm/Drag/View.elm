module Drag.View exposing (draggable)

import Drag.Messages as Drag
import Drag.State exposing (getPosition)
import Drag.Types exposing (Draggable, Position)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Main.Messages exposing (Msg(DragMsg))
import Mouse
import Util exposing (px)


draggable : Draggable a -> List (Attribute Msg)
draggable model =
    let
        pos : Position
        pos =
            getPosition model
    in
        [ on "mousedown" <| Json.map (DragMsg << Drag.Start) Mouse.position
        , style
            [ ( "position", "absolute" )
            , ( "top", px pos.y )
            , ( "left", px pos.x )
            ]
        ]
