module Settings.View exposing (view)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Main.Messages as Main
import Settings.Messages as Settings
import Settings.Types exposing (..)


view : Model -> List (Html Main.Msg) -> Html Main.Msg
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
                    Main.SettingsMsg <|
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
                                        Main.SetVolume <|
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
