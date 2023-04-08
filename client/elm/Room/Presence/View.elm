module Presence.View exposing (view)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Presence.Messages exposing (Msg(..))
import Presence.Types exposing (Model)


view : Model -> Html Msg
view model =
    div [ class "presence-box" ] <|
        case model.error of
            "" ->
                case model.presence of
                    Just users ->
                        [ div [ class "presence" ]
                            [ h1 [] [ text "CHALLENGERS" ]
                            , div [ class "presence-users" ] <|
                                List.map
                                    (\{ name, id } -> div [ class "presence-user", onClick <| Challenge id ] [ text name ])
                                <|
                                    List.take 4 users
                            ]
                        ]

                    Nothing ->
                        []

            _ ->
                [ div [ class "error" ] [ text model.error ] ]
