module Connected.View exposing (concedeView, htmlView, playersView, specMenuView, titleView, webglView)

import Assets.Types as Assets
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
import WebGL


htmlView : Model -> Flags -> Html Msg
htmlView { game, roomID, players } flags =
    div []
        [ playersView players
        , GameState.htmlView game roomID flags
        ]


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { game } flags assets =
    GameState.webglView game (GameState.paramsFromFlags flags) assets


playersView : Players -> Html Msg
playersView { pa, pb } =
    let
        playerView : Maybe String -> Bool -> Html msg
        playerView mName isMe =
            div
                [ classList [ ( "player-name ", True ), ( "me", isMe ) ] ]
                [ text <| Maybe.withDefault "" mName ]
    in
    div [ class "player-layer" ]
        [ playerView pa True
        , playerView pb False
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
                [ text "CONCEDE" ]
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


titleView : Model -> String
titleView { players } =
    let
        name : Maybe String -> String
        name mName =
            String.toUpper <| Maybe.withDefault "???" mName
    in
    name players.pb ++ " vs " ++ name players.pa
