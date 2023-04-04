module Notifications.View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Notifications.Messages exposing (Msg(..))
import Notifications.Types exposing (Model)


view : Model -> Html Main.Msg
view { notifications } =
    case List.head notifications of
        Just notification ->
            let
                clickHandler =
                    onClick (Main.NotificationsMsg Dismiss)
                        :: (case notification.callback of
                                Just callback ->
                                    [ onClick callback ]

                                Nothing ->
                                    []
                           )
            in
            div (class "notification" :: clickHandler) [ text notification.text ]

        Nothing ->
            text ""
