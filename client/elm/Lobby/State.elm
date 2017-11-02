module Lobby.State exposing (..)

import Lobby.Messages exposing (Msg(..))
import Lobby.Types exposing (GameType(..), Model)
import Main.Messages as Main
import Main.Types exposing (Mode(..))
import String exposing (dropLeft, length, startsWith)
import Util exposing (message)


modelInit : String -> GameType -> Model
modelInit roomID gameType =
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
        message <| Main.StartGame Playing
    else if startsWith "acceptSpec:" msg then
        message <| Main.StartGame Spectating
    else if startsWith "error:" msg then
        message <| Main.LobbyMsg <| JoinRoomErr <| dropLeft (length "error:") msg
    else
        -- Defer other messages.
        message <| Main.Receive <| msg
