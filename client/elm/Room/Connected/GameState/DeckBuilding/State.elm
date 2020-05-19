module DeckBuilding.State exposing (init, update)

import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Characters, Model, Rune, RuneSelectModel)
import Main.Messages as Main
import Ports exposing (log)
import Util


init : Character -> List Character -> List Rune -> Model
init character remaining runes =
    { characters =
        { previous = []
        , selected = character
        , remaining = remaining
        }
    , runes = runes
    , runeSelect = Nothing
    , ready = False
    }


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

        EnterRuneSelect _ excluded1 excluded2 index ->
            let
                runeSelect : RuneSelectModel
                runeSelect =
                    { excluded1 = excluded1, excluded2 = excluded2, index = index }
            in
            ( { model | runeSelect = Just runeSelect }, Cmd.none )

        UpdateRune rune index ->
            let
                { characters } =
                    model

                { selected } =
                    characters

                mNewSelected =
                    case index of
                        0 ->
                            Just { selected | runeA = rune }

                        1 ->
                            Just { selected | runeB = rune }

                        2 ->
                            Just { selected | runeC = rune }

                        _ ->
                            Nothing
            in
            case mNewSelected of
                Just newSelected ->
                    ( { model
                        | characters = { characters | selected = newSelected }
                        , runeSelect = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, log <| "rune select error " ++ String.fromInt index )


nextCharacter : Characters -> Characters
nextCharacter { previous, selected, remaining } =
    case remaining of
        next :: leftovers ->
            { previous = selected :: previous
            , selected = next
            , remaining = leftovers
            }

        _ ->
            nextCharacter
                { previous = []
                , selected = selected
                , remaining = List.reverse previous
                }


previousCharacter : Characters -> Characters
previousCharacter { previous, selected, remaining } =
    case previous of
        prev :: leftovers ->
            { previous = leftovers
            , selected = prev
            , remaining = selected :: remaining
            }

        _ ->
            previousCharacter
                { previous = List.reverse remaining
                , selected = selected
                , remaining = []
                }
