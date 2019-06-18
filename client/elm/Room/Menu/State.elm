module Menu.State exposing (update)

import Browser.Navigation
import GameType
import Main.Messages as Main
import Main.Types exposing (Flags)
import Menu.Messages exposing (Msg(..))


update : Msg -> Flags -> Cmd Main.Msg
update msg { key } =
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

                        GameType.DailyGame ->
                            "daily"
            in
            Browser.Navigation.pushUrl key <| "/play/" ++ url
