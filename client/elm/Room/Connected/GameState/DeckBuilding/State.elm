module DeckBuilding.State exposing (update)

import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (    Model)
import Main.Messages as Main
import Util


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        Select character ->
            let
                cmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ encodeCharacter character
            in
            ( { model | ready = True }, cmd )

        NextCharacter ->
            if not model.ready then
                ( { model | index = model.index + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousCharacter ->
            if not model.ready then
                ( { model | index = model.index - 1 }, Cmd.none )

            else
                ( model, Cmd.none )
