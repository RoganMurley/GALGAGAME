module Settings.View exposing (view)

import Html exposing (Html, button, div, img, input, label, text)
import Html.Attributes as H exposing (checked, class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Settings.Messages as Settings
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


view : Model -> Flags -> List (Html Main.Msg) -> List (Html Main.Msg) -> Html Main.Msg
view { modalState, musicVolume, sfxVolume } { backgroundEnabled, scaling } headerView buttonsView =
    let
        menuStyle =
            case modalState of
                Closed ->
                    style "display" "none"

                MenuOpen ->
                    style "" ""

        closeStyle =
            case modalState of
                Closed ->
                    style "display" "none"

                MenuOpen ->
                    style "" ""

        settingsIcon =
            case modalState of
                MenuOpen ->
                    img
                        [ class "settings-icon"
                        , src "/img/icon/close.svg"
                        , onClick <|
                            Main.SettingsMsg <|
                                Settings.CloseMenu
                        ]
                        []

                _ ->
                    img
                        [ class "settings-icon"
                        , src "/img/icon/hamburger.svg"
                        , onClick <|
                            Main.SettingsMsg <|
                                Settings.OpenMenu
                        ]
                        []
    in
    div
        [ class "settings-layer" ]
        [ div
            [ class "settings-close-background"
            , closeStyle
            , onClick <| Main.SettingsMsg Settings.CloseMenu
            ]
            []
        , settingsIcon
        , div [ class "settings-menu", menuStyle ] <|
            headerView
                ++ [ label [ class "settings-label" ]
                        [ text <| "Sound Effect Volume (" ++ String.fromInt sfxVolume ++ "%)"
                        , input
                            [ class "settings-slider"
                            , type_ "range"
                            , H.min "0"
                            , H.max "100"
                            , value <| String.fromInt sfxVolume
                            , onInput
                                (\v -> Main.SetVolume Sfx <| Maybe.withDefault 0 (String.toInt v))
                            ]
                            []
                        ]
                   , label [ class "settings-label" ]
                        [ text <| "Music Volume (" ++ String.fromInt musicVolume ++ "%)"
                        , input
                            [ class "settings-slider"
                            , type_ "range"
                            , H.min "0"
                            , H.max "100"
                            , value <| String.fromInt musicVolume
                            , onInput
                                (\v -> Main.SetVolume Music <| Maybe.withDefault 0 (String.toInt v))
                            ]
                            []
                        ]
                   , label [ class "settings-label" ]
                        [ text <| "Quality (" ++ String.fromFloat scaling ++ ")"
                        , input
                            [ class "settings-slider"
                            , type_ "range"
                            , H.min "0"
                            , H.max "2"
                            , H.step "0.1"
                            , value <| String.fromFloat scaling
                            , onInput
                                (\s -> Main.SetScaling <| Maybe.withDefault 1 (String.toFloat s))
                            ]
                            []
                        ]
                   , label [ class "settings-label" ]
                        [ text <| "Dynamic Background"
                        , input
                            [ class "settings-checkbox"
                            , type_ "checkbox"
                            , checked backgroundEnabled
                            , onClick <| Main.ToggleBackground <| not backgroundEnabled
                            ]
                            []
                        ]
                   , button
                        [ class "settings-button"
                        , onClick Main.GotoCustomGame
                        ]
                        [ text "PLAY WITH A FRIEND" ]
                   ]
                ++ buttonsView
        ]
