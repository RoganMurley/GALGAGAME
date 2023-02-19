module Room.State exposing (init, mouseDown, mouseUp, receive, tick, update, visibilityChange)

import Assets.Types as Assets
import Browser.Events exposing (Visibility)
import Browser.Navigation
import Connected.State as Connected
import Create.State as Create
import Feedback.State as Feedback
import GameType exposing (GameType(..))
import Leaderboard.State as Leaderboard
import League.State as League
import Lobby.State as Lobby
import Login.State as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.State as Menu
import Mode exposing (Mode(..))
import Mouse
import Presence.State as Presence
import Profile.State as Profile
import Replay.State as Replay
import Room.Messages exposing (Msg(..))
import Room.Types exposing (Model(..))
import Signup.State as Signup
import Util exposing (message)


init : Model
init =
    MainMenu


update : Model -> Msg -> Assets.Model -> Flags -> ( Model, Cmd Main.Msg )
update model msg assets flags =
    case msg of
        MenuMsg menuMsg ->
            case model of
                MainMenu ->
                    ( model, Menu.update menuMsg flags )

                _ ->
                    ( model, Cmd.none )

        LobbyMsg lobbyMsg ->
            case model of
                Lobby lobby ->
                    let
                        ( newLobby, newMsg ) =
                            Lobby.update lobby lobbyMsg flags
                    in
                    ( Lobby newLobby, newMsg )

                _ ->
                    ( model, Cmd.none )

        ConnectedMsg connectedMsg ->
            case model of
                Connected connected ->
                    let
                        ( newConnected, cmd ) =
                            Connected.update flags assets connectedMsg connected
                    in
                    ( Connected newConnected, cmd )

                _ ->
                    ( model, Cmd.none )

        LoginMsg loginMsg ->
            case model of
                Login login ->
                    let
                        ( newLogin, cmd ) =
                            Login.update login loginMsg flags
                    in
                    ( Login newLogin, cmd )

                _ ->
                    ( model, Cmd.none )

        ReplayMsg replayMsg ->
            case model of
                Replay replay ->
                    let
                        ( newReplay, cmd ) =
                            Replay.update replay replayMsg flags
                    in
                    ( Replay newReplay, cmd )

                _ ->
                    ( model, Cmd.none )

        SignupMsg signupMsg ->
            case model of
                Signup signup ->
                    let
                        ( newSignup, cmd ) =
                            Signup.update signup signupMsg flags
                    in
                    ( Signup newSignup, cmd )

                _ ->
                    ( model, Cmd.none )

        FeedbackMsg feedbackMsg ->
            case model of
                Feedback feedback ->
                    let
                        ( newFeedback, cmd ) =
                            Feedback.update feedback feedbackMsg flags
                    in
                    ( Feedback newFeedback, cmd )

                _ ->
                    ( model, Cmd.none )

        LeagueMsg leagueMsg ->
            case model of
                League league ->
                    let
                        ( newLeague, cmd ) =
                            League.update league leagueMsg flags
                    in
                    ( League newLeague, cmd )

                _ ->
                    ( model, Cmd.none )

        LeaderboardMsg leaderboardMsg ->
            case model of
                Leaderboard leaderboard ->
                    let
                        ( newLeaderboard, cmd ) =
                            Leaderboard.update leaderboard leaderboardMsg flags
                    in
                    ( Leaderboard newLeaderboard, cmd )

                _ ->
                    ( model, Cmd.none )

        ProfileMsg profileMsg ->
            case model of
                Profile profile ->
                    let
                        ( newProfile, cmd ) =
                            Profile.update profile profileMsg flags
                    in
                    ( Profile newProfile, cmd )

                _ ->
                    ( model, Cmd.none )

        PresenceMsg presenceMsg ->
            case model of
                Presence presence ->
                    let
                        ( newPresence, cmd ) =
                            Presence.update presence presenceMsg flags
                    in
                    ( Presence newPresence, cmd )

                _ ->
                    ( model, Cmd.none )

        CreateMsg createMsg ->
            case model of
                Create create ->
                    let
                        ( newCreate, cmd ) =
                            Create.update create createMsg flags
                    in
                    ( Create newCreate, cmd )

                _ ->
                    ( model, Cmd.none )

        StartGame mode messageRoomID ->
            case model of
                Lobby lobby ->
                    let
                        roomID =
                            Maybe.withDefault lobby.roomID messageRoomID
                    in
                    ( Connected <| Connected.init mode lobby.gameType roomID
                    , case ( lobby.gameType, mode ) of
                        ( GameType.CustomGame, Playing ) ->
                            Browser.Navigation.pushUrl flags.key <|
                                "/play/custom/"
                                    ++ roomID

                        ( GameType.ComputerGame, Playing ) ->
                            Browser.Navigation.pushUrl flags.key <|
                                "/play/computer/"
                                    ++ roomID

                        _ ->
                            Cmd.none
                    )

                Connected connected ->
                    let
                        roomID =
                            Maybe.withDefault connected.roomID messageRoomID
                    in
                    ( Connected <| Connected.init mode connected.gameType roomID
                    , message <| Main.Send "endEncounter:"
                    )

                _ ->
                    ( model, Cmd.none )


receive : Flags -> Assets.Model -> String -> Model -> ( Model, Cmd Main.Msg )
receive flags assets str model =
    case model of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Lobby.receive str )

        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.receive flags assets connected str
            in
            ( Connected newConnected, cmd )

        Replay replay ->
            ( Replay replay, Cmd.none )

        Login login ->
            ( Login login, Login.receive str )

        Signup signup ->
            ( Signup signup, Signup.receive str )

        Feedback feedback ->
            ( Feedback feedback, Feedback.receive str )

        League league ->
            ( League league, League.receive str )

        Leaderboard league ->
            ( Leaderboard league, Cmd.none )

        Profile profile ->
            ( Profile profile, Cmd.none )

        Presence presence ->
            ( Presence presence, Cmd.none )

        Create create ->
            ( Create create, Cmd.none )


tick : Flags -> Model -> Float -> ( Model, Cmd Msg )
tick flags room dt =
    case room of
        MainMenu ->
            ( MainMenu, Cmd.none )

        Lobby lobby ->
            ( Lobby lobby, Cmd.none )

        Login login ->
            ( Login login, Cmd.none )

        Connected connected ->
            let
                ( newConnected, msg ) =
                    Connected.tick flags connected dt
            in
            ( Connected newConnected, Cmd.map ConnectedMsg msg )

        Replay replay ->
            ( Replay <| Replay.tick flags replay dt, Cmd.none )

        Signup signup ->
            ( Signup signup, Cmd.none )

        Feedback feedback ->
            ( Feedback feedback, Cmd.none )

        League league ->
            ( League league, Cmd.none )

        Leaderboard leaderboard ->
            ( Leaderboard leaderboard, Cmd.none )

        Profile profile ->
            ( Profile profile, Cmd.none )

        Presence presence ->
            ( Presence presence, Cmd.none )

        Create create ->
            ( Create create, Cmd.none )


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp flags assets model pos =
    case model of
        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.mouseUp flags assets connected pos
            in
            ( Connected newConnected, cmd )

        Replay replay ->
            ( Replay <| Replay.mouseUp flags replay, Cmd.none )

        _ ->
            ( model, Cmd.none )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown flags assets model pos =
    case model of
        Connected connected ->
            let
                ( newConnected, cmd ) =
                    Connected.mouseDown flags assets connected pos
            in
            ( Connected newConnected, cmd )

        Replay replay ->
            let
                ( newReplay, cmd ) =
                    Replay.mouseDown flags assets replay pos
            in
            ( Replay newReplay, cmd )

        _ ->
            ( model, Cmd.none )


visibilityChange : Model -> Visibility -> Model
visibilityChange model _ =
    case model of
        Connected connected ->
            Connected { connected | heartbeatTick = 0 }

        _ ->
            model
