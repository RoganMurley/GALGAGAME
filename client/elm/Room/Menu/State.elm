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
                mUrl : Maybe String
                mUrl =
                    case gameType of
                        GameType.ComputerGame ->
                            Just "computer"

                        GameType.CustomGame ->
                            Just "custom"

                        GameType.QuickplayGame ->
                            Just "quickplay"

                        GameType.TutorialGame ->
                            Just "tutorial"

                        GameType.DailyGame ->
                            Just "daily"
            in
            case mUrl of
                Just url ->
                    Browser.Navigation.pushUrl key <| "/play/" ++ url

                Nothing ->
                    Cmd.none
