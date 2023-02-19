module Presence.View exposing (view)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Presence.Types exposing (Model)


view : Model -> Html a
view model =
    div [ class "presence-box" ] <|
        case model.error of
            "" ->
                case model.presence of
                    Just usernames ->
                        [ div [ class "presence" ]
                            [ h1 [] [ text "CHALLENGERS" ]
                            , div [ class "presence-users" ] <|
                                List.map
                                    (\username -> div [ class "presence-user" ] [ text username ])
                                    (List.take 4 usernames ++ [ "CPU" ])
                            ]
                        ]

                    Nothing ->
                        []

            _ ->
                [ div [ class "error" ] [ text model.error ] ]
