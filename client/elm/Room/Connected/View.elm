module Connected.View exposing (concedeView, playersView, specMenuView, view)

import Connected.Messages as Connected
import Connected.Types exposing (Model, Players)
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, classList, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import PlayState.Types exposing (PlayState(..))
import Texture.Types as Texture


view : Model -> Flags -> Texture.Model -> Html Msg
view { game, gameType, roomID, players } flags textures =
    div []
        [ playersView players
        , GameState.view game roomID flags (Just gameType) textures
        ]


playersView : Players -> Html Msg
playersView { pa, pb } =
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

        _ ->
            []


specMenuView : Flags -> Model -> List (Html Msg)
specMenuView { hostname, httpPort } { roomID } =
    let
        myID =
            "spec-link"

        portProtocol =
            if httpPort /= "" then
                ":" ++ httpPort

            else
                ""

        specLink =
            "https://" ++ hostname ++ portProtocol ++ "/spec/" ++ roomID
    in
    [ input
        [ value specLink
        , type_ "text"
        , readonly True
        , id myID
        , class "settings-input"
        , onClick <| SelectAllInput myID
        ]
        []
    ]
