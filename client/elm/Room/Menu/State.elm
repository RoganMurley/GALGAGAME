module Menu.State exposing (update)

import Lobby.Types as Lobby
import Main.Messages as Main
import Menu.Messages exposing (Msg(..))
import Navigation


update : Msg -> Cmd Main.Msg
update msg =
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
