module DeckBuilding.State exposing (getRuneFromCursor, init, mouseDown, nextCursor, tick, update)

import Assets.Types as Assets
import Audio.State exposing (playSound)
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import Chat.Messages as Chat
import Chat.Types as Chat
import Connected.Messages as Connected
import DeckBuilding.Decoders as Decoders
import DeckBuilding.Encoders exposing (encodeCharacter)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.Types exposing (Context)
import GameState.Messages as GameState
import Json.Decode as Json
import List.Extra as List
import Main.Messages as Main
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Mouse exposing (Position)
import Players exposing (Players)
import Ports exposing (getSavedCharacter, log, saveCharacter)
import Random
import Random.List as Random
import Room.Messages as Room
import RuneSelect.Messages as RuneSelect
import RuneSelect.State as RuneSelect
import RuneSelect.Types as RuneSelect exposing (Rune, RuneCursor(..))
import Set exposing (Set)
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


selectingMsg : Msg -> Main.Msg
selectingMsg =
    Main.RoomMsg
        << Room.ConnectedMsg
        << Connected.GameStateMsg
        << GameState.SelectingMsg


update : Msg -> Model -> Assets.Model -> Players -> ( Model, Cmd Main.Msg )
update msg model ({ audio } as assets) players =
    case msg of
        Select selectCharacter ->
            let
                selectCmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ encodeCharacter selectCharacter

                saveCmd =
                    saveCharacter <| encodeCharacter selectCharacter
            in
            ( { model | ready = True }
            , Cmd.batch
                [ selectCmd
                , saveCmd
                , playSound audio "sfx/click.mp3"
                ]
            )

        EnterRuneSelect cursor ->
            case model.character of
                Just character ->
                    case getRuneFromCursor cursor character of
                        Just rune ->
                            let
                                unlockedRuneNames : Set String
                                unlockedRuneNames =
                                    Maybe.map .unlocks players.pa
                                        |> Maybe.withDefault Set.empty

                                excludedRuneNames : Set String
                                excludedRuneNames =
                                    Set.fromList <|
                                        List.map .name <|
                                            List.filterMap identity <|
                                                [ Just rune
                                                , getRuneFromCursor (nextCursor cursor) character
                                                , getRuneFromCursor (nextCursor (nextCursor cursor)) character
                                                ]

                                carouselRuneNames : Set String
                                carouselRuneNames =
                                    Set.diff unlockedRuneNames excludedRuneNames

                                runeSelect : RuneSelect.Model
                                runeSelect =
                                    { cursor = cursor
                                    , carousel =
                                        Carousel.init rune <|
                                            List.filter
                                                (\r -> Set.member r.name carouselRuneNames)
                                                model.runes
                                    , entities = []
                                    , hover = Nothing
                                    , buttons = Buttons.empty
                                    }
                            in
                            ( { model | runeSelect = Just runeSelect }
                            , playSound audio "sfx/click.mp3"
                            )

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
                    , playSound audio "sfx/click.mp3"
                    )

                Nothing ->
                    ( model, Cmd.none )

        RandomRunes ->
            let
                unlockedRuneNames : Set String
                unlockedRuneNames =
                    Maybe.map .unlocks players.pa
                        |> Maybe.withDefault Set.empty

                legalRunes : List Rune
                legalRunes =
                    List.filter
                        (\rune -> Set.member rune.name unlockedRuneNames)
                        model.runes

                randomizer : List Rune -> Main.Msg
                randomizer runes =
                    selectingMsg <|
                        case runes of
                            runeA :: runeB :: runeC :: _ ->
                                SetRunes runeA runeB runeC

                            _ ->
                                Error <|
                                    "Rune randomizer failed because there were only "
                                        ++ String.fromInt (List.length runes)
                                        ++ " runes"

                newButtons =
                    Buttons.update
                        "random"
                        (\b -> { b | hover = 0 })
                        model.buttons
            in
            ( { model | buttons = newButtons }
            , Random.generate (randomizer << Tuple.first) (Random.choices 3 legalRunes)
            )

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
                        , pop = 0
                        }
              }
            , Cmd.none
            )

        RuneSelectMsg runeSelectMsg ->
            case model.runeSelect of
                Just runeSelect ->
                    let
                        ( newRoomSelect, cmd ) =
                            RuneSelect.update runeSelectMsg runeSelect assets
                    in
                    ( { model | runeSelect = Just newRoomSelect }, cmd )

                Nothing ->
                    ( model, log "RuneSelect message not on a RuneSelect game state" )

        LoadSavedCharacter ( saved, unlock ) ->
            let
                xp =
                    Maybe.map .xp players.pa
                        |> Maybe.withDefault 0

                fallbackMsg =
                    message <| selectingMsg RandomRunes
            in
            case saved of
                Nothing ->
                    ( model, Cmd.batch [ fallbackMsg, log "No saved character" ] )

                Just json ->
                    ( model
                    , case Json.decodeString Decoders.runeNames json of
                        Ok { a, b, c } ->
                            let
                                aOrUnlock =
                                    Maybe.withDefault a unlock

                                mRuneA =
                                    List.find (\r -> r.name == aOrUnlock && r.xp <= xp) model.runes

                                mRuneB =
                                    List.find (\r -> r.name == b && r.xp <= xp) model.runes

                                mRuneC =
                                    List.find (\r -> r.name == c && r.xp <= xp) model.runes
                            in
                            case [ mRuneA, mRuneB, mRuneC ] of
                                [ Just runeA, Just runeB, Just runeC ] ->
                                    message <|
                                        selectingMsg <|
                                            SetRunes runeA runeB runeC

                                _ ->
                                    Cmd.batch
                                        [ fallbackMsg
                                        , log "bad saved character"
                                        ]

                        Err err ->
                            Cmd.batch
                                [ fallbackMsg
                                , log <| Json.errorToString err
                                ]
                    )

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
                    getSavedCharacter ()

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
                , character = Maybe.map (characterTick dt) model.character
            }
    in
    ( newModel, msg )


maxPop : Float
maxPop =
    100


characterTick : Float -> Character -> Character
characterTick dt character =
    { character | pop = min maxPop <| character.pop + dt }


characterButtons : Context -> Float -> Chat.Model -> Model -> Buttons
characterButtons { radius, w, h, mouse } dt chat { ready, buttons, character } =
    let
        runePop =
            Maybe.withDefault 0 <| Maybe.map .pop character

        runeScale =
            radius * (0.3 + 0.03 * sin (pi * runePop / maxPop))

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
                            , options = [ Buttons.Circular, Buttons.IsIcon, Buttons.TextScale 0.65 ]
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
                            , text = "dice"
                            , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                            , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                            , options = [ Buttons.Circular, Buttons.IsIcon, Buttons.TextScale 1.2 ]
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


mouseDown : Position -> Players -> Assets.Model -> Model -> ( Model, Cmd Main.Msg )
mouseDown { x, y } players assets model =
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
                                    update (Select character) model assets players

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
                            let
                                ( newModel, newMsg ) =
                                    update RandomRunes model assets players
                            in
                            ( newModel
                            , Cmd.batch
                                [ newMsg
                                , playSound assets.audio "sfx/click.mp3"
                                ]
                            )

                        "runeA" ->
                            update (EnterRuneSelect RuneCursorA) model assets players

                        "runeB" ->
                            update (EnterRuneSelect RuneCursorB) model assets players

                        "runeC" ->
                            update (EnterRuneSelect RuneCursorC) model assets players

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Just runeSelect ->
            case Buttons.hit runeSelect.buttons pos of
                Just ( key, _ ) ->
                    case key of
                        "nextRune" ->
                            update (RuneSelectMsg RuneSelect.NextRune) model assets players

                        "prevRune" ->
                            update (RuneSelectMsg RuneSelect.PreviousRune) model assets players

                        "selectRune" ->
                            case model.runeSelect of
                                Just { cursor, carousel } ->
                                    update (ConfirmRune cursor carousel.selected) model assets players

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
