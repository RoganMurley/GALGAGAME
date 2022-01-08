module Connected.View exposing (concedeView, htmlView, playersView, specMenuView, titleView, webglView)

import Assets.Types as Assets
import Browser.Events exposing (Visibility(..))
import Chat.View as Chat
import Connected.Messages as Connected
import Connected.Types exposing (Model)
import GameState.Types exposing (GameState(..))
import GameState.View as GameState
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, classList, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import PlayState.Types exposing (PlayState(..))
import Players exposing (Players)
import Ripple.View as Ripple
import Room.Messages as Room
import WebGL


htmlView : Model -> Flags -> Html Msg
htmlView { chat, game, roomID, players, errored, connectionLost } flags =
    div []
        [ playersView players
        , GameState.htmlView game roomID flags
        , errorView errored connectionLost
        , Html.map
            (RoomMsg << Room.ConnectedMsg << Connected.ChatMsg)
            (Chat.htmlView flags chat)
        ]


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { chat, game, ripples } flags assets =
    let
        params =
            GameState.paramsFromFlags flags
    in
    List.concat
        [ GameState.webglView game chat params assets
        , Ripple.view ripples params assets
        ]


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


titleView : Flags -> Model -> String
titleView _ { players } =
    let
        name : Maybe String -> String
        name mName =
            String.toUpper <| Maybe.withDefault "???" mName
    in
    name players.pb ++ " vs " ++ name players.pa


errorView : Bool -> Bool -> Html Msg
errorView errored connectionLost =
    case ( errored, connectionLost ) of
        ( True, _ ) ->
            div [ class "connected-error" ] [ text "Something went wrong" ]

        ( _, True ) ->
            div [ class "connection-lost" ]
                [ h1 [] [ text "CONNECTION LOST" ]
                , button [ class "menu-button", onClick Reload ] [ text "RECONNECT" ]
                ]

        _ ->
            text ""
