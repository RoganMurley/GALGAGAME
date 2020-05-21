module Settings.View exposing (view)

import Html exposing (Html, div, h1, img, input, label, text)
import Html.Attributes as H exposing (class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Main.Messages as Main
import Settings.Messages as Settings
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


view : Model -> List (Html Main.Msg) -> Html Main.Msg
view { modalState, masterVolume } nestedViews =
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
                , label [ class "settings-volume" ]
                    [ text "Volume"
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
