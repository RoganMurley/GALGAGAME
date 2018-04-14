module Connected.View exposing (concedeView, playersView, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Connected.Messages as Connected
import Connected.Types exposing (..)
import GameState.Types exposing (GameState(..), PlayState(..))
import GameState.View as GameState
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import Texture.Types as Texture


view : Model -> Flags -> Texture.Model -> Html Msg
view { game, roomID, players } flags textures =
    div []
        [ playersView players
        , GameState.view game roomID flags textures
        ]


playersView : ( Maybe String, Maybe String ) -> Html Msg
playersView ( pa, pb ) =
    let
        playerView : Maybe String -> Bool -> Html msg
        playerView mName me =
            let
                meClass : String
                meClass =
                    if me then
                        "me"
                    else
                        ""
            in
                div
                    [ class <| "player-name " ++ meClass ]
                    [ text <| Maybe.withDefault "" mName ]
    in
        div [ class "player-layer" ]
            [ playerView pb True
            , playerView pa False
            ]


concedeView : GameState -> List (Html Connected.Msg)
concedeView state =
    case state of
        Started (Playing _) ->
            [ button
                [ classList
                    [ ( "settings-button", True )
                    , ( "settings-concede", True )
                    ]
                , onClick Connected.Concede
                ]
                [ text "Concede" ]
            ]

        otherwise ->
            []
