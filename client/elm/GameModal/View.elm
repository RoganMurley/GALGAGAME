module GameModal.View exposing (view)

import GameModal.Messages exposing (Msg(..))
import GameModal.Types exposing (Model(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    let
        modalStyle =
            case model of
                Closed ->
                    style [ ( "display", "none" ) ]

                Open ->
                    style []
    in
        div [ class "modal-layer" ]
            [ img
                [ class "settings-icon"
                , src "/img/icon/settings.svg"
                , onClick <| ToggleModal
                ]
                []
            , div [ modalStyle, class "modal-open" ]
                [ div [ class "modal-body" ]
                    [ div [ class "modal-inner" ]
                        [ h1 [] [ text "Settings" ]
                        ]
                    ]
                ]
            ]
