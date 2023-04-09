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
                    case notification.options of
                        [] ->
                            [ button
                                [ class "menu-button notification-button"
                                , onClick <| Main.NotificationsMsg Dismiss
                                ]
                                [ text "OK" ]
                            ]

                        _ ->
                            List.map
                                (\option ->
                                    button
                                        [ class "menu-button notification-button"
                                        , onClick option.msg
                                        ]
                                        [ text option.cta ]
                                )
                                notification.options
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
