module Main.View exposing (titleView, view)

import Html exposing (Html)
import Main.Messages exposing (Msg)
import Main.Types exposing (Model)
import Room.View as Room


view : Model -> Html Msg
view { room, settings, notifications, flags, assets } =
    Room.view room settings notifications flags assets


titleView : Model -> String
titleView { room } =
    Room.titleView room
