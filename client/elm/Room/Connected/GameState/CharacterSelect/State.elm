module CharacterSelect.State exposing (update, viewModelInit)

import CharacterSelect.Messages exposing (Msg(..))
import CharacterSelect.Types exposing (Character, Model, ViewModel)
import Main.Messages as Main
import Util


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ vm } as m) =
    case msg of
        Hover character ->
            ( { m | vm = { vm | hover = character } }, Cmd.none )

        Select { name } ->
            let
                cmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ name
            in
            ( m, cmd )


viewModelInit : Character -> ViewModel
viewModelInit character =
    { hover = character }
