module DeckBuilding.State exposing (getRuneFromCursor, init, nextCursor, update)

import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Characters, Model, Rune, RuneCursor(..), RuneSelectModel)
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
update msg ({ characters, runeSelect } as model) =
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
                ( { model | characters = nextCharacter characters }, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousCharacter ->
            if not model.ready then
                ( { model | characters = previousCharacter characters }, Cmd.none )

            else
                ( model, Cmd.none )

        EnterRuneSelect cursor ->
            let
                newRuneSelect : RuneSelectModel
                newRuneSelect =
                    { cursor = cursor
                    , selected = getRuneFromCursor cursor characters.selected
                    }
            in
            ( { model | runeSelect = Just newRuneSelect }, Cmd.none )

        SelectRune rune ->
            case runeSelect of
                Just rs ->
                    ( { model | runeSelect = Just { rs | selected = rune } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, log <| "SelectRune not on rune selecting state" )

        ConfirmRune ->
            case runeSelect of
                Just { cursor, selected } ->
                    let
                        newCharacter : Character
                        newCharacter =
                            setRuneFromCursor cursor selected characters.selected
                    in
                    ( { model
                        | characters =
                            { characters
                                | selected = newCharacter
                            }
                        , runeSelect = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, log <| "ConfirmRune not on rune selecting state" )


getRuneFromCursor : RuneCursor -> Character -> Rune
getRuneFromCursor cursor =
    case cursor of
        RuneCursorA ->
            .runeA

        RuneCursorB ->
            .runeB

        RuneCursorC ->
            .runeC


setRuneFromCursor : RuneCursor -> Rune -> Character -> Character
setRuneFromCursor cursor rune character =
    case cursor of
        RuneCursorA ->
            { character | runeA = rune }

        RuneCursorB ->
            { character | runeB = rune }

        RuneCursorC ->
            { character | runeC = rune }


nextCursor : RuneCursor -> RuneCursor
nextCursor cursor =
    case cursor of
        RuneCursorA ->
            RuneCursorB

        RuneCursorB ->
            RuneCursorC

        RuneCursorC ->
            RuneCursorA


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
