module Settings.View exposing (view)

import Html exposing (Html, div, h1, img, input, label, text)
import Html.Attributes as H exposing (class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Settings.Messages as Settings
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


view : Model -> Flags -> List (Html Main.Msg) -> Html Main.Msg
view { modalState, masterVolume } { scaling } nestedViews =
    let
        settingsStyle =
            case modalState of
                Closed ->
                    style "display" "none"

                Open ->
                    style "" ""
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
                [ h1 [] [ text "SETTINGS" ]
                , label [ class "settings-label" ]
                    [ text <| "Volume (" ++ String.fromInt masterVolume ++ "%)"
                    , input
                        [ class "settings-slider"
                        , type_ "range"
                        , H.min "0"
                        , H.max "100"
                        , value <| String.fromInt masterVolume
                        , onInput
                            (\v -> Main.SetVolume Master <| Maybe.withDefault 0 (String.toInt v))
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

                -- , label [ class "settings-label" ]
                --     [ text <| "Game Speed (" ++ String.fromFloat gameSpeed ++ ")"
                --     , input
                --         [ class "settings-slider"
                --         , type_ "range"
                --         , H.min "0.1"
                --         , H.max "2"
                --         , H.step "0.01"
                --         , value <| String.fromFloat gameSpeed
                --         , onInput
                --             (\s ->
                --                 Main.SettingsMsg <|
                --                     Settings.SetGameSpeed <|
                --                         Maybe.withDefault 1 (String.toFloat s)
                --             )
                --         ]
                --         []
                --     ]
                -- , label [ class "settings-volume" ]
                --     [ text "Music Volume"
                --     , input
                --         [ class "settings-slider"
                --         , type_ "range"
                --         , H.min "0"
                --         , H.max "100"
                --         , value <| String.fromInt musicVolume
                --         , onInput
                --             (\v -> Main.SetVolume Music <| Maybe.withDefault 0 (String.toInt v))
                --         ]
                --         []
                --     ]
                -- , label [ class "settings-volume" ]
                --     [ text "SFX Volume"
                --     , input
                --         [ class "settings-slider"
                --         , type_ "range"
                --         , H.min "0"
                --         , H.max "100"
                --         , value <| String.fromInt sfxVolume
                --         , onInput
                --             (\v -> Main.SetVolume Sfx <| Maybe.withDefault 0 (String.toInt v))
                --         ]
                --         []
                -- ]
                , div [] <|
                    -- onclick hack here as fullscreen requires user interaction.
                    List.concat
                        -- onclick was broken in elm 0.19
                        [ -- [ [ button
                          --         [ class "settings-button", attribute "onclick" "window.requestFullscreen()" ]
                          --         [ text "Fullscreen" ]
                          --   ]
                          -- , nestedViews
                          nestedViews
                        ]
                ]
            ]
        ]
