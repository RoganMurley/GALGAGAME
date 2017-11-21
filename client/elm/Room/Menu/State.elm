module Menu.State exposing (..)

import Lobby.Types as Lobby
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.Messages exposing (Msg(..))
import Navigation
import Room.Generators exposing (generate)


update : Msg -> Flags -> Cmd Main.Msg
update msg { seed } =
    let
        roomID : String
        roomID =
            generate Room.Generators.roomID seed
    in
        case msg of
            Start gameType ->
                let
                    url : String
                    url =
                        case gameType of
                            Lobby.ComputerGame ->
                                "computer"

                            Lobby.CustomGame ->
                                "custom"

                            Lobby.QuickplayGame ->
                                "quickplay"
                in
                    Navigation.newUrl <| "/play/" ++ url
