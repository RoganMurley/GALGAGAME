module Lobby.State exposing (gameTypeToString, init, receive, skipLobbyCmd, update)

import Browser.Navigation
import GameType exposing (GameType(..))
import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (LoginState(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Ports exposing (log, websocketReconnect)
import Room.Messages as Room
import Util exposing (message, splitOnColon)


maxJoinAttempts : Int
maxJoinAttempts =
    2


init : String -> Int -> GameType -> Mode -> Model
init roomID joinAttempts gameType mode =
    { roomID = roomID
    , joinAttempts = joinAttempts
    , error = ""
    , gameType = gameType
    , mode = mode
    , login = ChooseLoginOption
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update ({ gameType, mode, joinAttempts } as model) msg flags =
    case msg of
        JoinRoom force ->
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

                                ChallengeGame ->
                                    "playChallenge:"

                        Spectating ->
                            "spectate:"

                joinRoomCmd : List (Cmd Main.Msg)
                joinRoomCmd =
                    [ message <| Main.Send <| "room:" ++ model.roomID ]

                newMsg =
                    Cmd.batch <|
                        if joinAttempts > 0 && not force then
                            []

                        else
                            message (Main.Send prefix) :: joinRoomCmd
            in
            ( { model | joinAttempts = joinAttempts + 1 }
            , newMsg
            )

        JoinRoomErr error ->
            if joinAttempts >= maxJoinAttempts then
                ( { model | error = error }, Cmd.none )

            else
                ( model
                , Cmd.batch
                    [ websocketReconnect ()
                    , message <| Main.RoomMsg <| Room.LobbyMsg <| JoinRoom True
                    ]
                )

        SetRoom roomID ->
            ( { model | roomID = roomID }
            , Browser.Navigation.pushUrl flags.key <|
                "/play/quickplay/"
                    ++ roomID
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

        "room" ->
            message <|
                Main.RoomMsg <|
                    Room.LobbyMsg <|
                        SetRoom content

        _ ->
            -- Defer other messages.
            Cmd.batch
                [ message <|
                    Main.Receive <|
                        msg
                , log <| "Deferring lobby message " ++ msg
                ]


gameTypeToString : GameType -> String
gameTypeToString gameType =
    case gameType of
        CustomGame ->
            "CUSTOM"

        ComputerGame ->
            "CPU ROOM"

        QuickplayGame ->
            "GALGA"

        ChallengeGame ->
            "CHALLENGE"


skipLobbyCmd : Maybe String -> Cmd Main.Msg
skipLobbyCmd username =
    case username of
        Just _ ->
            message <|
                Main.RoomMsg <|
                    Room.LobbyMsg <|
                        JoinRoom False

        Nothing ->
            Cmd.none
