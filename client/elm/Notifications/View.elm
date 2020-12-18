module Notifications.View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Notifications.Messages exposing (Msg(..))
import Notifications.Types exposing (Model)


view : Model -> Html Msg
view { notifications } =
    case List.head notifications of
        Just notification ->
            div [ class "notification", onClick Dismiss ] [ text notification.text ]

        Nothing ->
            text ""
