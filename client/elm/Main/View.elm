module Main.View exposing (..)

import Html exposing (Html, div)
import Main.Messages exposing (Msg)
import Main.Types exposing (Model)
import Room.View as Room


view : Model -> Html Msg
view { room, settings, flags } =
    Room.view room settings flags
