module Entrypoint.View exposing (view)

import Entrypoint.Messages exposing (Msg(..))
import Entrypoint.Types exposing (Model)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


view : Model -> Html Msg
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
                                    [ class "presence-button", onClick <| Challenge id ]
                                    [ text <| "CHALLENGE " ++ name ]
                            )
                         <|
                            List.take 4 users
                        )
                            ++ [ button
                                    [ class "presence-button"
                                    , onClick Quickplay
                                    ]
                                    [ text "QUICKPLAY" ]
                               , button
                                    [ class "presence-button"
                                    , onClick CustomGame
                                    ]
                                    [ text "CUSTOM GAME" ]
                               ]
                    ]
                ]

            _ ->
                [ div [ class "error" ] [ text model.error ] ]
