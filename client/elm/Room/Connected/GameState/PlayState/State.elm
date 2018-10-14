module PlayState.State exposing (..)

import Audio exposing (playSound)
import Game.State as Game
import Game.Types as Game
import Json.Decode as Json
import List.Extra as List
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode)
import Model.Decoders as Model
import Model.State as Model
import Model.Types exposing (Model)
import Model.ViewModel
import Mouse exposing (Position)
import Navigation
import PlayState.Decoders as PlayState
import PlayState.Encoders
import PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))
import PlayState.Types as PlayState exposing (PlayState(..))
import Ports exposing (reload)
import Resolvable.Decoders
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Util exposing (message, send, unsafeForceDecode)
import WhichPlayer.Types exposing (WhichPlayer(..))


update : Msg -> PlayState -> Mode -> Flags -> ( PlayState, Cmd Main.Msg )
update msg state mode flags =
    case msg of
        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode flags

        HoverSelf i ->
            case state of
                Playing ({ game } as playing) ->
                    let
                        { res } =
                            playing.game

                        { vm } =
                            res

                        newVm : Model.ViewModel.ViewModel
                        newVm =
                            { vm | hover = i }
                    in
                        ( Playing
                            { playing
                                | game =
                                    { game
                                        | res =
                                            { res | vm = newVm }
                                    }
                            }
                        , Cmd.none
                        )

                s ->
                    ( s, Cmd.none )

        HoverOutcome i ->
            case state of
                Playing ({ game } as playing) ->
                    let
                        { res } =
                            game

                        { final } =
                            res

                        newFinal : Model
                        newFinal =
                            { final | otherHover = i }
                    in
                        ( Playing
                            { playing
                                | game =
                                    { game
                                        | res =
                                            { res
                                                | final = newFinal
                                            }
                                    }
                            }
                        , Cmd.none
                        )

                _ ->
                    ( state, Cmd.none )

        GotoReplay replayId ->
            state
                ! [ Navigation.newUrl <| "/replay/" ++ replayId
                  , reload ()
                  ]

        ReplaySaved replayId ->
            case state of
                Ended ended ->
                    ( Ended { ended | replayId = Just replayId }, Cmd.none )

                _ ->
                    ( state, Cmd.none )


updatePlayingOnly : PlayingOnly -> PlayState -> Mode.Mode -> Flags -> ( PlayState, Cmd Main.Msg )
updatePlayingOnly msg state mode flags =
    case mode of
        Mode.Spectating ->
            ( state, Cmd.none )

        Mode.Playing ->
            case msg of
                Rematch ->
                    case state of
                        Ended _ ->
                            ( state, send flags "rematch:" )

                        _ ->
                            ( state, Cmd.none )

                HoverCard mIndex ->
                    let
                        ( newState, cmd ) =
                            update (HoverSelf mIndex) state mode flags

                        sound =
                            case mIndex of
                                Nothing ->
                                    Cmd.none

                                _ ->
                                    playSound "/sfx/hover.wav"
                    in
                        newState
                            ! [ cmd
                              , message <|
                                    Main.Send <|
                                        "hover:"
                                            ++ PlayState.Encoders.hoverIndex mIndex
                              , sound
                              ]

                TurnOnly turnOnly ->
                    updateTurnOnly turnOnly state mode flags


updateTurnOnly : TurnOnly -> PlayState -> Mode.Mode -> Flags -> ( PlayState, Cmd Main.Msg )
updateTurnOnly msg state mode flags =
    let
        legal =
            case state of
                Playing { game } ->
                    game.res.final.turn == PlayerA

                _ ->
                    False
    in
        if not legal then
            ( state, Cmd.none )
        else
            case msg of
                EndTurn ->
                    state
                        ! [ send flags "end:"
                          , playSound "/sfx/endTurn.wav"
                          ]

                PlayCard index ->
                    let
                        ( newState, cmd ) =
                            update (HoverSelf Nothing) state mode flags
                    in
                        newState
                            ! [ send flags <| "play:" ++ toString index
                              , playSound "/sfx/playCard.wav"
                              , cmd
                              ]


tick : Flags -> PlayState -> Float -> PlayState
tick flags state dt =
    map (Game.tick flags dt) state


map : (Game.Model -> Game.Model) -> PlayState -> PlayState
map f state =
    case state of
        Playing playing ->
            Playing { playing | game = f playing.game }

        Ended ended ->
            Ended { ended | game = f ended.game }


get : (Game.Model -> a) -> PlayState -> a
get g state =
    case state of
        Playing { game } ->
            g game

        Ended { game } ->
            g game


carryVm : PlayState -> PlayState -> PlayState
carryVm old new =
    let
        oldVm : Model.ViewModel.ViewModel
        oldVm =
            .vm <| get .res old

        newRes : Resolvable.Model
        newRes =
            get .res new

        oldEntities : Game.Entities
        oldEntities =
            get .entities old
    in
        map
            (\game ->
                { game
                    | res =
                        { newRes
                            | vm = oldVm
                        }
                    , entities = oldEntities
                }
            )
            new


resolveOutcome : String -> Maybe PlayState -> PlayState
resolveOutcome str mState =
    let
        state : PlayState
        state =
            Maybe.withDefault
                (Playing { game = Game.gameInit Model.init })
                mState

        oldResList : List Resolvable.ResolveData
        oldResList =
            get (.res >> .resList) state

        oldTick : Float
        oldTick =
            case oldResList of
                [] ->
                    0

                _ ->
                    get (.res >> .tick) state

        initial : Model
        initial =
            unsafeForceDecode (Json.field "initial" Model.decoder) str

        resDiffList : List Resolvable.ResolveDiffData
        resDiffList =
            unsafeForceDecode
                (Json.field "list" <|
                    Json.list Resolvable.Decoders.resolveDiffData
                )
                str

        resList : List Resolvable.ResolveData
        resList =
            Resolvable.resDiffToData initial resDiffList

        finalState : PlayState
        finalState =
            unsafeForceDecode (Json.field "final" PlayState.decoder) str

        model : Model
        model =
            get (.res >> .final) finalState

        res : Resolvable.Model
        res =
            { vm = Model.ViewModel.init
            , tick = oldTick
            , final = model
            , resList = oldResList ++ resList
            }

        newState : PlayState
        newState =
            map (\game -> { game | res = res }) finalState
    in
        carryVm state newState


mouseMove : Position -> PlayState -> PlayState
mouseMove { x, y } state =
    let
        pos =
            vec2 (toFloat x) (toFloat y)
    in
        map (\game -> { game | mouse = pos }) state


mouseClick : Mode -> Flags -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseClick mode flags { x, y } state =
    let
        pos =
            vec2 (toFloat x) (toFloat y)
    in
        case state of
            Playing { game } ->
                let
                    mIndex =
                        Maybe.map .index <|
                            List.find
                                (Game.hitTest pos 28)
                                game.entities.hand
                in
                    case mIndex of
                        Just index ->
                            update
                                (PlayingOnly <| TurnOnly <| PlayCard index)
                                state
                                mode
                                flags

                        Nothing ->
                            ( state, Cmd.none )

            _ ->
                ( state, Cmd.none )
