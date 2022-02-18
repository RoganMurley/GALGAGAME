module DeckBuilding.State exposing (getRuneFromCursor, init, mouseDown, nextCursor, tick, update)

import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import Chat.Messages as Chat
import Chat.Types as Chat
import Connected.Messages as Connected
import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.Types exposing (Context)
import GameState.Messages as GameState
import Main.Messages as Main
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Mouse exposing (Position)
import Players exposing (Players)
import Ports exposing (log)
import Random
import Random.List as Random
import Room.Messages as Room
import RuneSelect.Messages as RuneSelect
import RuneSelect.State as RuneSelect
import RuneSelect.Types as RuneSelect exposing (Rune, RuneCursor(..))
import Util exposing (message)
import Vfx.State as Vfx


init : Bool -> Maybe Character -> List Rune -> Model
init ready character runes =
    { character = character
    , runes = runes
    , runeSelect = Nothing
    , ready = ready
    , bounceTick = 0
    , vfx = Vfx.init
    , buttons = Buttons.empty
    }


update : Msg -> Model -> Players -> ( Model, Cmd Main.Msg )
update msg model players =
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
            case model.character of
                Just character ->
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

                                xp =
                                    Maybe.map .xp players.pa
                                        |> Maybe.withDefault 0

                                runeSelect : RuneSelect.Model
                                runeSelect =
                                    { cursor = cursor
                                    , carousel =
                                        Carousel.init
                                            rune
                                        <|
                                            List.filter (\r -> r.xp <= xp) <|
                                                List.filter (\r -> not (List.member r excludedRunes))
                                                    model.runes
                                    , entities = []
                                    , hover = Nothing
                                    , buttons = Buttons.empty
                                    }
                            in
                            ( { model | runeSelect = Just runeSelect }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ConfirmRune cursor rune ->
            case model.character of
                Just character ->
                    ( { model
                        | character = Just <| setRuneFromCursor cursor rune character
                        , runeSelect = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RandomRunes ->
            let
                xp =
                    Maybe.map .xp players.pa
                        |> Maybe.withDefault 0

                legalRunes =
                    List.filter (\rune -> rune.xp <= xp) model.runes

                randomizer : List Rune -> Main.Msg
                randomizer runes =
                    Main.RoomMsg <|
                        Room.ConnectedMsg <|
                            Connected.GameStateMsg <|
                                GameState.SelectingMsg <|
                                    case runes of
                                        runeA :: runeB :: runeC :: _ ->
                                            SetRunes runeA runeB runeC

                                        _ ->
                                            Error <|
                                                "Rune randomizer failed because there were only "
                                                    ++ String.fromInt (List.length runes)
                                                    ++ " runes"
            in
            ( model, Random.generate (randomizer << Tuple.first) (Random.choices 3 legalRunes) )

        SetRunes runeA runeB runeC ->
            ( { model
                | character =
                    Just
                        { choice =
                            Just
                                { runeA = runeA
                                , runeB = runeB
                                , runeC = runeC
                                }
                        }
              }
            , Cmd.none
            )

        RuneSelectMsg runeSelectMsg ->
            case model.runeSelect of
                Just runeSelect ->
                    ( { model | runeSelect = Just <| RuneSelect.update runeSelectMsg runeSelect }, Cmd.none )

                Nothing ->
                    ( model, log "RuneSelect message not on a RuneSelect game state" )

        Error str ->
            ( model, log str )


tick : Context -> Float -> Chat.Model -> Model -> ( Model, Cmd Msg )
tick ctx dt chat model =
    let
        newRuneSelect =
            Maybe.map (RuneSelect.tick ctx dt) model.runeSelect

        msg =
            case model.character of
                Just _ ->
                    Cmd.none

                Nothing ->
                    message RandomRunes

        newModel =
            { model
                | runeSelect = newRuneSelect
                , bounceTick = model.bounceTick + dt
                , vfx = Vfx.tick dt model.vfx Nothing ctx
                , buttons =
                    case model.runeSelect of
                        Just runeSelect ->
                            RuneSelect.buttons ctx dt runeSelect

                        Nothing ->
                            characterButtons ctx dt chat model
            }
    in
    ( newModel, msg )


characterButtons : Context -> Float -> Chat.Model -> Model -> Buttons
characterButtons { radius, w, h, mouse } dt chat { ready, buttons, character } =
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
                , Buttons.entity "toggleChat"
                    { x = w * 0.5 - 0.5 * radius
                    , y = 0.8 * h
                    , width = 0.12 * radius
                    , height = 0.12 * radius
                    , btn =
                        TextButton
                            { font = "Futura"
                            , text =
                                if chat.visible then
                                    "chatClose"

                                else
                                    "chat"
                            , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                            , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                            , options = [ Buttons.Circular, Buttons.IsIcon ]
                            }
                    , disabled = False
                    }
                , Buttons.entity "random"
                    { x = w * 0.5 + 0.5 * radius
                    , y = 0.8 * h
                    , width = 0.12 * radius
                    , height = 0.12 * radius
                    , btn =
                        TextButton
                            { font = "Futura"
                            , text = "?"
                            , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                            , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                            , options = [ Buttons.Circular ]
                            }
                    , disabled = False
                    }
                ]
                    ++ (case Maybe.map .choice character |> Maybe.join of
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


mouseDown : Position -> Players -> Model -> ( Model, Cmd Main.Msg )
mouseDown { x, y } players model =
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
                            case model.character of
                                Just character ->
                                    update (Select character) model players

                                Nothing ->
                                    ( model, Cmd.none )

                        "toggleChat" ->
                            ( model
                            , message <|
                                Main.RoomMsg <|
                                    Room.ConnectedMsg <|
                                        Connected.ChatMsg <|
                                            Chat.ToggleVisibility
                            )

                        "random" ->
                            update RandomRunes model players

                        "runeA" ->
                            update (EnterRuneSelect RuneCursorA) model players

                        "runeB" ->
                            update (EnterRuneSelect RuneCursorB) model players

                        "runeC" ->
                            update (EnterRuneSelect RuneCursorC) model players

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Just runeSelect ->
            case Buttons.hit runeSelect.buttons pos of
                Just ( key, _ ) ->
                    case key of
                        "nextRune" ->
                            update (RuneSelectMsg RuneSelect.NextRune) model players

                        "prevRune" ->
                            update (RuneSelectMsg RuneSelect.PreviousRune) model players

                        "selectRune" ->
                            case model.runeSelect of
                                Just { cursor, carousel } ->
                                    update (ConfirmRune cursor carousel.selected) model players

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
