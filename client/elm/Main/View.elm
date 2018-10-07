module Main.View exposing (..)

import Html exposing (Html)
import Main.Messages exposing (Msg)
import Main.Types exposing (Model)
import Room.View as Room


view : Model -> Html Msg
view { room, settings, flags, textures } =
    Room.view room settings flags textures
