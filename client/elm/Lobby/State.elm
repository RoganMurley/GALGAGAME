module Lobby.State exposing (..)

import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (GameType(..), Model)
import Main.Messages as Main
import Connected.Types exposing (Mode(..))
import Lobby.Messages as Lobby
import Room.Messages as Room
import String exposing (dropLeft, length, startsWith)
import Util exposing (message)


init : String -> GameType -> Model
init roomID gameType =
    { roomID = roomID
    , error = ""
    , gameType = gameType
    }


update : Model -> Msg -> ( Model, Cmd Main.Msg )
update ({ error, gameType } as model) msg =
    case msg of
        JoinRoom mode ->
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
    if startsWith "acceptPlay:" msg then
        message <|
            Main.RoomMsg <|
                Room.StartGame Playing
    else if startsWith "acceptSpec:" msg then
        message <|
            Main.RoomMsg <|
                Room.StartGame Spectating
    else if startsWith "error:" msg then
        message <|
            Main.RoomMsg <|
                Room.LobbyMsg <|
                    Lobby.JoinRoomErr <|
                        dropLeft (length "error:") msg
    else
        -- Defer other messages.
        message <| Main.Receive <| msg
