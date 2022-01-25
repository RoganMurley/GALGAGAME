module Settings.View exposing (view)

import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes as H exposing (class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Settings.Messages as Settings
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


view : Model -> Flags -> List (Html Main.Msg) -> Html Main.Msg
view { modalState, musicVolume, sfxVolume } { scaling } nestedViews =
    let
        settingsStyle =
            case modalState of
                Closed ->
                    style "display" "none"

                ModalOpen ->
                    style "" ""

                MenuOpen ->
                    style "display" "none"

        menuStyle =
            case modalState of
                Closed ->
                    style "display" "none"

                ModalOpen ->
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
                                Settings.Close
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
    div [ class "settings-layer" ]
        [ settingsIcon
        , div
            [ settingsStyle
            , class "settings-open"
            ]
            [ div
                [ class "settings-body"
                ]
                [ h1 [] [ text "SETTINGS" ]
                , label [ class "settings-label" ]
                    [ text <| "SFX Volume (" ++ String.fromInt sfxVolume ++ "%)"
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
                , div [] <|
                    List.concat
                        [ nestedViews
                        ]
                ]
            ]
        , div [ class "hamburger-menu", menuStyle ] <|
            [ label [ class "settings-label" ]
                [ text <| "SFX Volume (" ++ String.fromInt sfxVolume ++ "%)"
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
            , button
                [ class "hamburger-button"
                , onClick Main.GotoCustomGame
                ]
                [ text "PLAY WITH FRIEND" ]
            ]
                ++ nestedViews
        ]
