module DeckBuilding.State exposing (getRuneFromCursor, init, mouseClick, nextCursor, tick, update)

import Carousel
import Collision exposing (hitTest)
import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Button, Character, Model)
import Game.Types exposing (Context)
import Main.Messages as Main
import Math.Vector2 exposing (vec2)
import Mouse exposing (Position)
import Ports exposing (log)
import RuneSelect.State as RuneSelect
import RuneSelect.Types as RuneSelect exposing (Rune, RuneCursor(..))
import Util
import Vfx.State as Vfx


init : Character -> List Character -> List Rune -> Model
init character remaining runes =
    { characters = Carousel.init character remaining
    , runes = runes
    , runeSelect = Nothing
    , ready = False
    , bounceTick = 0
    , vfx = Vfx.init
    , buttons = { ready = Nothing }
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ characters } as model) =
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
                ( { model | characters = Carousel.forward characters, bounceTick = 0 }, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousCharacter ->
            if not model.ready then
                ( { model | characters = Carousel.backward characters, bounceTick = 0 }, Cmd.none )

            else
                ( model, Cmd.none )

        EnterRuneSelect cursor ->
            let
                rune : Rune
                rune =
                    getRuneFromCursor cursor characters.selected

                excludedRunes : List Rune
                excludedRunes =
                    [ rune
                    , getRuneFromCursor (nextCursor cursor) characters.selected
                    , getRuneFromCursor (nextCursor (nextCursor cursor)) characters.selected
                    ]

                runeSelect : RuneSelect.Model
                runeSelect =
                    { cursor = cursor
                    , carousel =
                        Carousel.init
                            rune
                            (List.filter (\r -> not (List.member r excludedRunes)) model.runes)
                    , entities = []
                    , hover = Nothing
                    }
            in
            ( { model | runeSelect = Just runeSelect }, Cmd.none )

        ConfirmRune cursor rune ->
            let
                newCharacter : Character
                newCharacter =
                    setRuneFromCursor cursor rune characters.selected
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

        RuneSelectMsg runeSelectMsg ->
            case model.runeSelect of
                Just runeSelect ->
                    ( { model | runeSelect = Just <| RuneSelect.update runeSelectMsg runeSelect }, Cmd.none )

                Nothing ->
                    ( model, log "RuneSelect message not on a RuneSelect game state" )


tick : Context -> Float -> Model -> Model
tick ctx dt model =
    let
        newRuneSelect =
            Maybe.map (RuneSelect.tick ctx dt) model.runeSelect
    in
    { model
        | runeSelect = newRuneSelect
        , bounceTick = model.bounceTick + dt
        , vfx = Vfx.tick dt model.vfx ctx
        , buttons =
            { ready =
                if model.ready then
                    Nothing

                else
                    Just <| readyButton model.buttons.ready dt ctx
            }
    }


readyButton : Maybe Button -> Float -> Context -> Button
readyButton mButton dt { mouse, w, h } =
    let
        x =
            w * 0.5

        y =
            h * 0.8

        previousHover =
            case mButton of
                Just button ->
                    button.hover

                Nothing ->
                    0

        hover =
            case mouse of
                Just m ->
                    if hitTest m 100 { position = vec2 x y } then
                        min (previousHover + dt) 300

                    else
                        0

                Nothing ->
                    0
    in
    { x = x
    , y = y
    , hover = hover
    , size = 0.6 * max w h
    }


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


mouseClick : Position -> Model -> ( Model, Cmd Main.Msg )
mouseClick _ model =
    case model.runeSelect of
        Nothing ->
            let
                hit =
                    case model.buttons.ready of
                        Just { hover } ->
                            hover > 0

                        Nothing ->
                            False
            in
            if hit then
                update (Select model.characters.selected) model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
