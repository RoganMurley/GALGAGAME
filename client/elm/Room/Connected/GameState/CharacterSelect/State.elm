module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import CharacterSelect.Messages exposing (Msg(..))
import Main.Messages as Main
import Mode exposing (Mode)
import Util


update : Msg -> Model -> Mode -> ( Model, Cmd Main.Msg )
update msg model mode =
    case msg of
        Hover character ->
            ( { model | hover = character }, Cmd.none )

        Select { name } ->
            let
                cmd : Cmd Main.Msg
                cmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ name
            in
                ( model, cmd )
