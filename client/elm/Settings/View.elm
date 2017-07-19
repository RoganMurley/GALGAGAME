module Settings.View exposing (view)

import Settings.Messages exposing (Msg(..))
import Main.Messages as Main
import Settings.Types exposing (Model(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : Model -> Html Main.Msg
view model =
    let
        settingsStyle =
            case model of
                Closed ->
                    style [ ( "display", "none" ) ]

                Open ->
                    style []
    in
        div [ class "settings-layer" ]
            [ img
                [ class "settings-icon"
                , src "/img/icon/settings.svg"
                , onClick <| Main.SettingsMsg <| ToggleSettings
                ]
                []
            , div
                [ settingsStyle
                , class "settings-open"
                , onClick <| Main.SettingsMsg <| CloseSettings
                ]
                [ div
                    [ class "settings-body"
                    ]
                    [ div [ class "settings-inner" ]
                        [ h1 [] [ text "Settings" ]
                        , button
                            [ class "settings-button"
                            , onClick <| Main.Concede
                            ]
                            [ text "Concede" ]
                        ]
                    ]
                ]
            ]
