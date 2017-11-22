module Lobby.State exposing (..)

import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (GameType(..), Model)
import Main.Messages as Main
import Mode exposing (Mode(..))
import Room.Messages as Room
import Util exposing (message, splitOn)


init : String -> GameType -> Mode -> Model
init roomID gameType mode =
    { roomID = roomID
    , error = ""
    , gameType = gameType
    , mode = mode
    }


update : Model -> Msg -> ( Model, Cmd Main.Msg )
update ({ error, gameType, mode } as model) msg =
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
            ( { model | error = error }, Cmd.none )


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOn ":" msg
    in
        case command of
            "acceptPlay" ->
                message <|
                    Main.RoomMsg <|
                        Room.StartGame Playing

            "acceptSpec" ->
                message <|
                    Main.RoomMsg <|
                        Room.StartGame Spectating

            "error" ->
                message <|
                    Main.RoomMsg <|
                        Room.LobbyMsg <|
                            JoinRoomErr <|
                                content

            otherwise ->
                -- Defer other messages.
                message <|
                    Main.Receive <|
                        msg


gameTypeToString : GameType -> String
gameTypeToString gameType =
    case gameType of
        CustomGame ->
            "Custom"

        ComputerGame ->
            "Computer"

        QuickplayGame ->
            "Quickplay"
