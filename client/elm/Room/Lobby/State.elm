module Lobby.State exposing (gameTypeToString, init, receive, skipLobbyCmd, update)

import Browser.Navigation
import GameType exposing (GameType(..))
import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (LoginState(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Ports exposing (websocketReconnect)
import Room.Messages as Room
import Util exposing (message, splitOnColon)


maxJoinAttempts : Int
maxJoinAttempts =
    2


init : String -> GameType -> Mode -> Model
init roomID gameType mode =
    { roomID = roomID
    , joinAttempts = 0
    , error = ""
    , gameType = gameType
    , mode = mode
    , login = ChooseLoginOption
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update ({ gameType, mode } as model) msg flags =
    case msg of
        JoinRoom ->
            let
                prefix : String
                prefix =
                    case mode of
                        Playing ->
                            case gameType of
                                CustomGame ->
                                    "play:"

                                ComputerGame ->
                                    "playComputer:"

                                QuickplayGame ->
                                    "queue:"

                                TutorialGame ->
                                    "playTutorial:"

                                DailyGame ->
                                    "playDaily:"

                        Spectating ->
                            "spectate:"
            in
            ( model
            , Cmd.batch
                [ message <| Main.Send <| prefix
                , message <| Main.Send <| "room:" ++ model.roomID
                ]
            )

        JoinRoomErr error ->
            let
                joinAttempts =
                    model.joinAttempts + 1
            in
            if joinAttempts >= maxJoinAttempts then
                ( { model | error = error, joinAttempts = joinAttempts }, Cmd.none )

            else
                ( { model | joinAttempts = joinAttempts }
                , Cmd.batch
                    [ websocketReconnect ()
                    , message <| Main.RoomMsg <| Room.LobbyMsg <| JoinRoom
                    ]
                )

        GotoLogin ->
            ( model
            , Browser.Navigation.pushUrl flags.key "/login"
            )

        GotoSignup ->
            ( model
            , Browser.Navigation.pushUrl flags.key "/signup"
            )


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "acceptPlay" ->
            message <|
                Main.RoomMsg <|
                    Room.StartGame Playing Nothing

        "acceptSpec" ->
            message <|
                Main.RoomMsg <|
                    Room.StartGame Spectating Nothing

        "error" ->
            message <|
                Main.RoomMsg <|
                    Room.LobbyMsg <|
                        JoinRoomErr <|
                            content

        _ ->
            -- Defer other messages.
            message <|
                Main.Receive <|
                    msg


gameTypeToString : GameType -> String
gameTypeToString gameType =
    case gameType of
        CustomGame ->
            "PRIVATE ROOM"

        ComputerGame ->
            "CPU ROOM"

        QuickplayGame ->
            "MATCHMAKING"

        TutorialGame ->
            "TUTORIAL"

        DailyGame ->
            "DAILY CHALLENGE"


skipLobbyCmd : Maybe String -> Cmd Main.Msg
skipLobbyCmd username =
    case username of
        Just _ ->
            message <|
                Main.RoomMsg <|
                    Room.LobbyMsg
                        JoinRoom

        Nothing ->
            Cmd.none
