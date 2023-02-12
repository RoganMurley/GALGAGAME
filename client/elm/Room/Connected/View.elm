module Connected.View exposing (concedeView, htmlView, playersView, settingsHeaderView, specMenuView, titleView, webglView)

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
import Players exposing (Player, Players)
import Ripple.View as Ripple
import Room.Messages as Room
import WebGL


htmlView : Model -> Flags -> Assets.Model -> Html Msg
htmlView { chat, game, roomID, players, errored, connectionLost } flags assets =
    div []
        [ playersView players
        , GameState.htmlView game roomID flags
        , errorView errored connectionLost
        , Html.map
            (RoomMsg << Room.ConnectedMsg << Connected.ChatMsg)
            (Chat.htmlView flags assets chat)
        ]


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { chat, game, ripples, players } flags assets =
    let
        params =
            GameState.paramsFromFlags flags
    in
    List.concat
        [ GameState.webglView game players chat params assets False
        , Ripple.view ripples params assets
        ]


playersView : Players -> Html a
playersView { pa, pb } =
    let
        playerView : Maybe Player -> Bool -> Html msg
        playerView player isMe =
            div
                [ classList [ ( "player-name ", True ), ( "me", isMe ) ] ]
                [ text <| Maybe.withDefault "" <| Maybe.map .name player ]
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
                [ class "settings-button"
                , class "settings-concede"
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
        name : Maybe Player -> String
        name player =
            String.toUpper <| Maybe.withDefault "???" <| Maybe.map .name player
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


settingsHeaderView : Flags -> Model -> List (Html Msg)
settingsHeaderView flags model =
    [ h1 [ class "settings-header" ] [ text <| titleView flags model ]
    ]
