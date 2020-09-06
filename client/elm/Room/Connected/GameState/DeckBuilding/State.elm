module DeckBuilding.State exposing (getRuneFromCursor, init, mouseDown, nextCursor, tick, update)

import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.Types exposing (Context)
import Main.Messages as Main
import Math.Vector3 exposing (vec3)
import Mouse exposing (Position)
import Ports exposing (log)
import RuneSelect.Messages as RuneSelect
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
    , buttons = Buttons.empty
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ characters, buttons } as model) =
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
            let
                newButtons =
                    Buttons.update
                        "next"
                        (\b -> { b | hover = 0 })
                        buttons
            in
            if not model.ready then
                ( { model
                    | characters =
                        Carousel.forward characters
                    , bounceTick = 0
                    , buttons = newButtons
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PreviousCharacter ->
            let
                newButtons =
                    Buttons.update
                        "prev"
                        (\b -> { b | hover = 0 })
                        buttons
            in
            if not model.ready then
                ( { model
                    | characters = Carousel.backward characters
                    , bounceTick = 0
                    , buttons = newButtons
                  }
                , Cmd.none
                )

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
                    , buttons = Buttons.empty
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
            case model.runeSelect of
                Just runeSelect ->
                    RuneSelect.buttons ctx dt runeSelect

                Nothing ->
                    characterButtons ctx dt model
    }


characterButtons : Context -> Float -> Model -> Buttons
characterButtons { w, h, mouse } dt { ready, buttons, characters } =
    let
        runeScale =
            0.9 * max w h

        triangleSide =
            runeScale * 0.06
    in
    if ready then
        Buttons.empty

    else
        Buttons.fromList <|
            List.map (\f -> f dt mouse buttons)
                [ Buttons.entity
                    "ready"
                    { x = 0.5 * w
                    , y = 0.8 * h
                    , xScale = 1.4 * max w h
                    , yScale = 0.6 * max w h
                    , btn =
                        TextButton
                            { font = "Futura"
                            , text = "Ready?"
                            , textColor = vec3 (0 / 255) (0 / 255) (80 / 255)
                            , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                            , options = [ Buttons.HoverText "Ready!" ]
                            }
                    , disabled = False
                    }
                , Buttons.entity
                    "next"
                    { x = 0.7 * w
                    , y = 0.5 * h
                    , xScale = 0.6 * max w h
                    , yScale = 0.6 * max w h
                    , btn =
                        ImageButton
                            { img = "next.png"
                            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                            }
                    , disabled = False
                    }
                , Buttons.entity
                    "prev"
                    { x = 0.3 * w
                    , y = 0.5 * h
                    , xScale = -0.6 * max w h
                    , yScale = 0.6 * max w h
                    , btn =
                        ImageButton
                            { img = "next.png"
                            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                            }
                    , disabled = False
                    }
                , Buttons.entity
                    "runeA"
                    { x = 0.5 * w
                    , y = 0.5 * h - triangleSide
                    , xScale = runeScale
                    , yScale = runeScale
                    , btn =
                        ImageButton
                            { img = characters.selected.runeA.imgURL
                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                            }
                    , disabled = False
                    }
                , Buttons.entity
                    "runeB"
                    { x = 0.5 * w + triangleSide / sin 1.04
                    , y = 0.5 * h + triangleSide
                    , xScale = runeScale
                    , yScale = runeScale
                    , btn =
                        ImageButton
                            { img = characters.selected.runeB.imgURL
                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                            }
                    , disabled = False
                    }
                , Buttons.entity
                    "runeC"
                    { x = 0.5 * w - triangleSide / sin 1.04
                    , y = 0.5 * h + triangleSide
                    , xScale = runeScale
                    , yScale = runeScale
                    , btn =
                        ImageButton
                            { img = characters.selected.runeC.imgURL
                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                            }
                    , disabled = False
                    }
                ]


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


mouseDown : Position -> Model -> ( Model, Cmd Main.Msg )
mouseDown _ model =
    case model.runeSelect of
        Nothing ->
            case Buttons.hit model.buttons of
                Just ( key, _ ) ->
                    case key of
                        "ready" ->
                            update (Select model.characters.selected) model

                        "next" ->
                            update NextCharacter model

                        "prev" ->
                            update PreviousCharacter model

                        "runeA" ->
                            update (EnterRuneSelect RuneCursorA) model

                        "runeB" ->
                            update (EnterRuneSelect RuneCursorB) model

                        "runeC" ->
                            update (EnterRuneSelect RuneCursorC) model

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Just runeSelect ->
            case Buttons.hit runeSelect.buttons of
                Just ( key, _ ) ->
                    case key of
                        "nextRune" ->
                            update (RuneSelectMsg RuneSelect.NextRune) model

                        "prevRune" ->
                            update (RuneSelectMsg RuneSelect.PreviousRune) model

                        "selectRune" ->
                            case model.runeSelect of
                                Just { cursor, carousel } ->
                                    update (ConfirmRune cursor carousel.selected) model

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
