module Settings.View exposing (view)

import Settings.Messages exposing (Msg(..))
import Main.Messages as Main
import Settings.Types exposing (..)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)


view : Model -> Html Main.Msg
view { modalState, volume } =
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
                , onClick <| Main.SettingsMsg <| ToggleSettings
                ]
                []
            , div
                [ settingsStyle
                , class "settings-open"
                  -- , onClick <| Main.SettingsMsg <| CloseSettings
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
                        , button
                            [ class "settings-button"
                            , onClick Main.Concede
                            ]
                            [ text "Concede" ]
                        ]
                    ]
                ]
            ]
