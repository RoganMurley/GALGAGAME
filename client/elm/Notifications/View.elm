module Notifications.View exposing (view)

import Html exposing (Html, button, div, h1, text)
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
                buttons =
                    case notification.callback of
                        Just callback ->
                            [ button
                                [ class "menu-button notification-button"
                                , onClick callback
                                , onClick <| Main.NotificationsMsg Dismiss
                                ]
                                [ text "ACCEPT" ]
                            , button
                                [ class "menu-button notification-button"
                                , onClick <| Main.NotificationsMsg Dismiss
                                ]
                                [ text "DECLINE" ]
                            ]

                        Nothing ->
                            [ button
                                [ class "menu-button notification-button"
                                , onClick <| Main.NotificationsMsg Dismiss
                                ]
                                [ text "OKAY" ]
                            ]
            in
            div [ class "notification-background" ]
                [ div
                    [ class "notification" ]
                    [ h1 [] [ text notification.text ]
                    , div [ class "notification-buttons" ] buttons
                    ]
                ]

        Nothing ->
            text ""
