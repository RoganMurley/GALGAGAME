module DeckBuilding.State exposing (getRuneFromCursor, init, mouseDown, nextCursor, tick, update)

import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.Types exposing (Context)
import Main.Messages as Main
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Mouse exposing (Position)
import Ports exposing (log)
import RuneSelect.Messages as RuneSelect
import RuneSelect.State as RuneSelect
import RuneSelect.Types as RuneSelect exposing (Rune, RuneCursor(..))
import Util
import Vfx.State as Vfx


init : Bool -> Character -> List Rune -> Model
init ready character runes =
    { character = character
    , runes = runes
    , runeSelect = Nothing
    , ready = ready
    , bounceTick = 0
    , vfx = Vfx.init
    , buttons = Buttons.empty
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ character } as model) =
    case msg of
        Select selectCharacter ->
            let
                cmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ encodeCharacter selectCharacter
            in
            ( { model | ready = True }, cmd )

        EnterRuneSelect cursor ->
            case getRuneFromCursor cursor character of
                Just rune ->
                    let
                        excludedRunes : List Rune
                        excludedRunes =
                            List.filterMap identity <|
                                [ Just rune
                                , getRuneFromCursor (nextCursor cursor) character
                                , getRuneFromCursor (nextCursor (nextCursor cursor)) character
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

                Nothing ->
                    ( model, Cmd.none )

        ConfirmRune cursor rune ->
            ( { model
                | character = setRuneFromCursor cursor rune character
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
characterButtons { radius, w, h, mouse } dt { ready, buttons, character } =
    let
        runeScale =
            radius * 0.3

        triangleSide =
            radius * 0.27
    in
    if ready then
        Buttons.empty

    else
        Buttons.fromList <|
            List.map (\f -> f dt mouse buttons) <|
                [ Buttons.entity
                    "ready"
                    { x = 0.5 * w
                    , y = 0.8 * h
                    , width = 0.25 * radius
                    , height = 0.1 * radius
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
                ]
                    ++ (case character.choice of
                            Nothing ->
                                []

                            Just choice ->
                                [ Buttons.entity
                                    "runeA"
                                    { x = 0.5 * w
                                    , y = 0.5 * h - triangleSide
                                    , width = runeScale
                                    , height = runeScale
                                    , btn =
                                        ImageButton
                                            { img = choice.runeA.imgURL
                                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                                            }
                                    , disabled = False
                                    }
                                , Buttons.entity
                                    "runeB"
                                    { x = 0.5 * w + triangleSide / sin 1.04
                                    , y = 0.5 * h + triangleSide
                                    , width = runeScale
                                    , height = runeScale
                                    , btn =
                                        ImageButton
                                            { img = choice.runeB.imgURL
                                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                                            }
                                    , disabled = False
                                    }
                                , Buttons.entity
                                    "runeC"
                                    { x = 0.5 * w - triangleSide / sin 1.04
                                    , y = 0.5 * h + triangleSide
                                    , width = runeScale
                                    , height = runeScale
                                    , btn =
                                        ImageButton
                                            { img = choice.runeC.imgURL
                                            , color = vec3 (255 / 255) (255 / 255) (255 / 255)
                                            }
                                    , disabled = False
                                    }
                                ]
                       )


getRuneFromCursor : RuneCursor -> Character -> Maybe Rune
getRuneFromCursor cursor character =
    let
        f =
            case cursor of
                RuneCursorA ->
                    .runeA

                RuneCursorB ->
                    .runeB

                RuneCursorC ->
                    .runeC
    in
    Maybe.map f character.choice


setRuneFromCursor : RuneCursor -> Rune -> Character -> Character
setRuneFromCursor cursor rune character =
    let
        f choice =
            case cursor of
                RuneCursorA ->
                    { choice | runeA = rune }

                RuneCursorB ->
                    { choice | runeB = rune }

                RuneCursorC ->
                    { choice | runeC = rune }
    in
    { character | choice = Maybe.map f character.choice }


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
mouseDown { x, y } model =
    let
        pos =
            vec2 (toFloat x) (toFloat y)
    in
    case model.runeSelect of
        Nothing ->
            case Buttons.hit model.buttons pos of
                Just ( key, _ ) ->
                    case key of
                        "ready" ->
                            update (Select model.character) model

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
            case Buttons.hit runeSelect.buttons pos of
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
