module DeckBuilding.State exposing (update)

import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Characters, Model)
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
                ( { model | characters = nextCharacter model.characters }, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousCharacter ->
            if not model.ready then
                ( { model | characters = previousCharacter model.characters }, Cmd.none )

            else
                ( model, Cmd.none )


nextCharacter : Characters -> Characters
nextCharacter { previous, selected, remaining } =
    case ( remaining, previous ) of
        ( next :: leftovers, _ ) ->
            { previous = selected :: previous
            , selected = next
            , remaining = leftovers
            }

        ( _, next :: leftovers ) ->
            { previous = selected :: previous
            , selected = next
            , remaining = leftovers
            }

        _ ->
            { previous = previous
            , selected = selected
            , remaining = remaining
            }


previousCharacter : Characters -> Characters
previousCharacter { previous, selected, remaining } =
    case ( remaining, previous ) of
        ( prev :: leftovers, _ ) ->
            { previous = leftovers
            , selected = prev
            , remaining = selected :: remaining
            }

        ( _, prev :: leftovers ) ->
            { previous = leftovers
            , selected = prev
            , remaining = selected :: remaining
            }

        _ ->
            { previous = previous
            , selected = selected
            , remaining = remaining
            }
