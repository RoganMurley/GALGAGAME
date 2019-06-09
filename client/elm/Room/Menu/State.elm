module Menu.State exposing (update)

import GameType
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
                        GameType.ComputerGame ->
                            "computer"

                        GameType.CustomGame ->
                            "custom"

                        GameType.QuickplayGame ->
                            "quickplay"

                        GameType.TutorialGame ->
                            "tutorial"
            in
            Navigation.newUrl <| "/play/" ++ url
