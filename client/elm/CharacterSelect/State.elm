module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import CharacterSelect.Messages exposing (Msg(..))
import Main.Messages as Main
import Util


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        Hover character ->
            ( { model | hover = character }, Cmd.none )

        Select { name } ->
            let
                cmd : Cmd Main.Msg
                cmd =
                    Util.message <|
                        Main.PlayingOnly <|
                            Main.Send <|
                                "selectCharacter:"
                                    ++ name
            in
                ( model, cmd )
