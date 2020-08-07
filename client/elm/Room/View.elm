module Room.View exposing (titleView, view, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Connected.View as Connected
import Feedback.View as Feedback
import GameState.View as GameState
import Html as Html exposing (Html, div)
import Html.Attributes exposing (class, height, width)
import Lobby.View as Lobby
import Login.View as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.View as Menu
import Replay.View as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Settings.Types as Settings
import Settings.View as Settings
import Signup.View as Signup
import WebGL


view : Model -> Settings.Model -> Flags -> Assets.Model -> Html Main.Msg
view model settings flags assets =
    let
        roomView =
            case model of
                MainMenu ->
                    Html.map (Main.RoomMsg << MenuMsg) <|
                        Menu.view

                Lobby lobby ->
                    Html.map (Main.RoomMsg << LobbyMsg) <|
                        Lobby.view lobby

                Connected connected ->
                    Connected.htmlView connected flags

                Replay replay ->
                    Replay.htmlView replay

                Login login ->
                    Html.map (Main.RoomMsg << LoginMsg) <|
                        Login.view login

                Signup signup ->
                    Html.map (Main.RoomMsg << SignupMsg) <|
                        Signup.view signup

                Feedback feedback ->
                    Html.map (Main.RoomMsg << FeedbackMsg) <|
                        Feedback.view feedback
    in
    div []
        [ Settings.view settings (settingsView model flags)
        , roomView
        , webglView model flags assets
        ]


settingsView : Model -> Flags -> List (Html Main.Msg)
settingsView model flags =
    let
        baseViews : List (Html Main.Msg)
        baseViews =
            Login.logoutView flags
    in
    case model of
        Connected connected ->
            List.concat
                [ baseViews
                , List.map (Html.map (Main.RoomMsg << ConnectedMsg)) <|
                    Connected.concedeView connected.game
                , Connected.specMenuView flags connected
                ]

        _ ->
            baseViews


titleView : Model -> String
titleView model =
    case model of
        Connected connected ->
            Connected.titleView connected ++ " | GALGAGAME"

        Replay _ ->
            "Replay | GALGAGAME"

        Login _ ->
            "Login | GALGAGAME"

        Signup _ ->
            "Signup | GALGAGAME"

        _ ->
            "GALGAGAME"


webglView : Model -> Flags -> Assets.Model -> Html Main.Msg
webglView model flags assets =
    let
        params =
            GameState.paramsFromFlags flags

        entities =
            case model of
                Connected connected ->
                    Connected.webglView connected flags assets

                Replay replay ->
                    Replay.webglView replay flags assets

                _ ->
                    Background.webglView params assets NullAnim
    in
    div []
        [ WebGL.toHtml
            [ width <| floor <| toFloat params.w * params.pixelRatio
            , height <| floor <| toFloat params.h * params.pixelRatio
            , class "webgl-canvas"
            ]
            entities
        ]
