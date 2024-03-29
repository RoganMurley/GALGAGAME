module Room.View exposing (titleView, view, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Connected.View as Connected
import Create.View as Create
import Entrypoint.View as Entrypoint
import Feedback.View as Feedback
import GameState.View as GameState
import Html as Html exposing (Html, div)
import Html.Attributes exposing (class, height, width)
import Leaderboard.View as Leaderboard
import League.View as League
import Lobby.View as Lobby
import Login.View as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Notifications.Types as Notifications
import Notifications.View as Notifications
import Profile.View as Profile
import Replay.View as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Settings.Types as Settings
import Settings.View as Settings
import Signup.View as Signup
import WebGL


view : Model -> Settings.Model -> Notifications.Model -> Flags -> Assets.Model -> Html Main.Msg
view model settings notifications flags assets =
    let
        roomView =
            case model of
                Lobby lobby ->
                    Lobby.view flags lobby

                Connected connected ->
                    Connected.htmlView connected flags assets

                Replay replay ->
                    Html.map (Main.RoomMsg << ReplayMsg) <|
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

                League league ->
                    Html.map (Main.RoomMsg << LeagueMsg) <|
                        League.view league

                Leaderboard leaderboard ->
                    Leaderboard.view leaderboard True

                Profile profile ->
                    Profile.view profile

                Entrypoint entrypoint ->
                    Entrypoint.view entrypoint

                Create create ->
                    Html.map (Main.RoomMsg << CreateMsg) <|
                        Create.view create

        ( settingsHeader, settingsButtons ) =
            settingsView model flags
    in
    div []
        [ Notifications.view notifications
        , Settings.view settings flags settingsHeader settingsButtons
        , roomView
        , webglView model flags assets
        ]


settingsView : Model -> Flags -> ( List (Html Main.Msg), List (Html Main.Msg) )
settingsView model flags =
    let
        baseButtons : List (Html Main.Msg)
        baseButtons =
            Login.loginoutView flags
    in
    case model of
        Connected connected ->
            ( Connected.settingsHeaderView flags connected
            , List.concat
                [ baseButtons
                , List.map (Html.map (Main.RoomMsg << ConnectedMsg)) <|
                    Connected.concedeView connected.game
                , Connected.specMenuView flags connected
                ]
            )

        _ ->
            ( [], baseButtons )


titleView : Flags -> Model -> String
titleView flags model =
    case model of
        Connected connected ->
            Connected.titleView flags connected ++ " | GALGA"

        Replay _ ->
            "Replay | GALGA"

        Login _ ->
            "Login | GALGA"

        Signup _ ->
            "Signup | GALGA"

        _ ->
            "GALGA"


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

        options =
            List.concat
                [ [ WebGL.alpha True, WebGL.depth 1 ]
                , if params.pixelRatio < 2 then
                    [ WebGL.antialias ]

                  else
                    []
                ]
    in
    div []
        [ WebGL.toHtmlWith options
            [ width <| floor <| toFloat params.w * params.pixelRatio * params.scaling
            , height <| floor <| toFloat params.h * params.pixelRatio * params.scaling
            , class "webgl-canvas"
            ]
            entities
        ]
