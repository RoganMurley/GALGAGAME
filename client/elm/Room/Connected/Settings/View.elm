module Settings.View exposing (view)

import Connected.Messages as Connected
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Settings.Messages as Settings
import Settings.Types exposing (..)


view : Model -> List (Html Connected.Msg) -> Html Connected.Msg
view { modalState, volume } nestedViews =
    let
        settingsStyle =
            case modalState of
                Closed ->
                    style [ ( "display", "none" ) ]

                Open ->
                    style []
    in
        div [ class "settings-layer" ]
            [ img
                [ class "settings-icon"
                , src "/img/icon/settings.svg"
                , onClick <|
                    Connected.SettingsMsg <|
                        Settings.ToggleSettings
                ]
                []
            , div
                [ settingsStyle
                , class "settings-open"
                ]
                [ div
                    [ class "settings-body"
                    ]
                    [ div [ class "settings-inner" ]
                        [ h1 [] [ text "Settings" ]
                        , label [ class "settings-volume" ]
                            [ text "Master Volume"
                            , input
                                [ class "settings-slider"
                                , type_ "range"
                                , H.min "0"
                                , H.max "100"
                                , value <| toString volume
                                , onInput
                                    (\v ->
                                        Connected.SetVolume <|
                                            Result.withDefault 0 (String.toInt v)
                                    )
                                ]
                                []
                            ]
                        , div [] nestedViews
                        ]
                    ]
                ]
            ]
