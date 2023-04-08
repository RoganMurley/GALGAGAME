module Entrypoint.View exposing (view)

import Entrypoint.Types exposing (Model)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Main.Messages as Main


view : Model -> Html Main.Msg
view model =
    div [ class "presence-box" ] <|
        case model.error of
            "" ->
                let
                    users =
                        Maybe.withDefault [] model.presence
                in
                [ div [ class "presence" ] <|
                    [ div [ class "presence-users" ] <|
                        (List.map
                            (\{ name, id } ->
                                button
                                    [ class "presence-button", onClick <| Main.Challenge id ]
                                    [ text <| "CHALLENGE " ++ name ]
                            )
                         <|
                            List.take 4 users
                        )
                            ++ [ button
                                    [ class "presence-button"
                                    , onClick Main.GotoQuickplayGame
                                    ]
                                    [ text "QUICKPLAY" ]
                               , button
                                    [ class "presence-button"
                                    , onClick <| Main.GotoCustomGame Nothing
                                    ]
                                    [ text "CUSTOM GAME" ]
                               ]
                    ]
                ]

            _ ->
                [ div [ class "error" ] [ text model.error ] ]
