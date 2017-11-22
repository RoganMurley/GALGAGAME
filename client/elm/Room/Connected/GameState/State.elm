module GameState.State exposing (activeAnim, resTick, update, tick, tickZero, gameTickStart)

import Audio exposing (playSound)
import Card.Types exposing (Anim)
import CharacterSelect.State as CharacterSelect
import Connected.Messages as Connected
import GameState.Decoders exposing (decodeState, resDecoder)
import GameState.Encoders exposing (encodeHoverIndex)
import GameState.Messages exposing (..)
import GameState.Types exposing (GameState(..))
import Json.Decode as Json exposing (field, maybe)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mode exposing (Mode(..))
import Model.Types exposing (..)
import Room.Messages as Room
import Model.ViewModel
import Util exposing (message, safeTail, send)


update : Msg -> GameState -> Mode -> Flags -> ( GameState, Cmd Main.Msg )
update msg state mode flags =
    case msg of
        Sync str ->
            let
                syncedState : GameState
                syncedState =
                    syncState state str
            in
                case state of
                    PlayingGame _ ( res, _ ) ->
                        -- If we're resolving, defer update until later.
                        case res of
                            [] ->
                                ( syncedState, Cmd.none )

                            otherwise ->
                                ( state
                                , message <|
                                    Main.RoomMsg <|
                                        Room.ConnectedMsg <|
                                            Connected.GameStateMsg <|
                                                msg
                                )

                    otherwise ->
                        ( syncedState, Cmd.none )

        HoverSelf i ->
            case state of
                PlayingGame ( m, vm ) r ->
                    ( PlayingGame ( m, { vm | hover = i } ) r, Cmd.none )

                s ->
                    ( s, Cmd.none )

        HoverOutcome i ->
            case state of
                PlayingGame ( m, vm ) r ->
                    ( PlayingGame ( { m | otherHover = i }, vm ) r, Cmd.none )

                s ->
                    ( s, Cmd.none )

        ResolveOutcome str ->
            let
                ( final, resList ) =
                    case Json.decodeString (resDecoder state) str of
                        Ok result ->
                            result

                        Err err ->
                            Debug.crash err
            in
                case resList of
                    [] ->
                        ( setRes final []
                        , Cmd.none
                        )

                    otherwise ->
                        case ( state, final ) of
                            ( PlayingGame ( oldModel, oldVm ) _, PlayingGame ( newModel, _ ) _ ) ->
                                ( resTick <| PlayingGame ( oldModel, oldVm ) ( resList ++ [ newModel ], 0 )
                                , Cmd.none
                                )

                            ( PlayingGame ( oldModel, oldVm ) _, Ended w f _ _ _ ) ->
                                ( resTick <| Ended w f oldVm (Just oldModel) ( resList, 0 )
                                , Cmd.none
                                )

                            otherwise ->
                                ( resTick <| setRes final resList
                                , Cmd.none
                                )

        SelectingMsg selectMsg ->
            case state of
                Selecting m ->
                    let
                        ( newModel, cmd ) =
                            CharacterSelect.update selectMsg m mode
                    in
                        ( Selecting newModel, cmd )

                otherwise ->
                    Debug.log
                        "Expected a selecting state"
                        ( state, Cmd.none )

        Shake mag ->
            case state of
                PlayingGame ( m, vm ) r ->
                    ( PlayingGame ( m, { vm | shake = mag } ) r, Cmd.none )

                otherwise ->
                    ( state, Cmd.none )

        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode flags


updatePlayingOnly : PlayingOnly -> GameState -> Mode -> Flags -> ( GameState, Cmd Main.Msg )
updatePlayingOnly msg state mode ({ hostname } as flags) =
    let
        legal =
            case mode of
                Playing ->
                    True

                Spectating ->
                    False
    in
        if not legal then
            ( state, Cmd.none )
        else
            case msg of
                Rematch ->
                    case state of
                        Ended _ _ _ _ _ ->
                            ( state, send hostname "rematch:" )

                        otherwise ->
                            ( state, Cmd.none )

                HoverCard mIndex ->
                    let
                        ( newState, cmd ) =
                            update (HoverSelf mIndex) state mode flags

                        sound =
                            case mIndex of
                                Nothing ->
                                    Cmd.none

                                otherwise ->
                                    playSound "/sfx/hover.wav"
                    in
                        ( newState
                        , Cmd.batch
                            [ cmd
                            , message <|
                                Main.Send <|
                                    "hover:"
                                        ++ (encodeHoverIndex mIndex)
                            , sound
                            ]
                        )

                TurnOnly turnOnly ->
                    updateTurnOnly turnOnly state mode flags


updateTurnOnly : TurnOnly -> GameState -> Mode -> Flags -> ( GameState, Cmd Main.Msg )
updateTurnOnly msg state mode ({ hostname } as flags) =
    let
        legal =
            case state of
                PlayingGame ( { turn }, _ ) _ ->
                    turn == PlayerA

                otherwise ->
                    False
    in
        if not legal then
            ( state, Cmd.none )
        else
            case msg of
                EndTurn ->
                    ( state
                    , Cmd.batch
                        [ send hostname "end:"
                        , playSound "/sfx/endTurn.wav"
                        ]
                    )

                PlayCard index ->
                    let
                        ( newState1, cmd1 ) =
                            update (HoverSelf Nothing) state mode flags

                        ( newState2, cmd2 ) =
                            update (Shake 1.0) newState1 mode flags
                    in
                        ( newState2
                        , Cmd.batch
                            [ send hostname ("play:" ++ (toString index))
                            , playSound "/sfx/playCard.wav"
                            , cmd1
                            , cmd2
                            ]
                        )


setRes : GameState -> List Model -> GameState
setRes state res =
    case state of
        PlayingGame ( m, vm ) ( _, i ) ->
            PlayingGame ( m, vm ) ( res, i )

        Ended w f vm m ( _, i ) ->
            Ended w f vm m ( res, i )

        Waiting _ ->
            Debug.log
                "Set res on a waiting state"
                state

        Selecting _ ->
            Debug.log
                "Set res on a Selecting state"
                state


syncState : GameState -> String -> GameState
syncState oldState msg =
    case (decodeState msg oldState) of
        Ok newState ->
            carryVm oldState newState

        Err err ->
            Debug.log
                err
                oldState



-- Carry the old viewmodel between a new and old GameState


carryVm : GameState -> GameState -> GameState
carryVm old new =
    case old of
        PlayingGame ( _, vm ) _ ->
            case new of
                PlayingGame ( m, _ ) ( res, i ) ->
                    PlayingGame ( m, vm ) ( res, i )

                otherwise ->
                    new

        otherwise ->
            new


resTick : GameState -> GameState
resTick state =
    let
        shakeMag : Float
        shakeMag =
            1.1
    in
        case state of
            PlayingGame ( model, vm ) ( res, _ ) ->
                case List.head res of
                    Just newModel ->
                        PlayingGame
                            ( newModel, { vm | shake = shakeMag } )
                            ( safeTail res, 0 )

                    Nothing ->
                        PlayingGame
                            ( model, vm )
                            ( res, 0 )

            Ended which final vm (Just m) ( res, _ ) ->
                Ended
                    which
                    final
                    { vm | shake = shakeMag }
                    (List.head res)
                    ( List.drop 1 res, 0 )

            otherwise ->
                state


resTickMax : Float
resTickMax =
    800.0


tickZero : Float -> Bool
tickZero tick =
    tick > resTickMax


tick : GameState -> Float -> GameState
tick game dt =
    case game of
        PlayingGame ( m, vm ) ( res, tick ) ->
            if tickZero tick then
                resTick game
            else
                PlayingGame ( m, (Model.ViewModel.shakeDecay vm) ) ( res, tick + dt )

        Ended which final vm resModel ( res, tick ) ->
            if tickZero tick then
                resTick game
            else
                Ended which final (Model.ViewModel.shakeDecay vm) resModel ( res, tick + dt )

        otherwise ->
            game


gameTickStart : GameState -> Bool
gameTickStart game =
    case game of
        PlayingGame ( _, _ ) ( _, 0.0 ) ->
            True

        Ended _ _ _ _ ( _, 0.0 ) ->
            True

        otherwise ->
            False



-- TOTALLY REDO THIS AWFUL CODE


activeAnim : GameState -> Maybe ( WhichPlayer, Anim )
activeAnim game =
    case game of
        PlayingGame ( model, _ ) ( _, _ ) ->
            case List.head model.stack of
                Just { card, owner } ->
                    Maybe.map ((,) owner) card.anim

                Nothing ->
                    Nothing

        Ended _ _ _ (Just model) ( _, _ ) ->
            case List.head model.stack of
                Just { card, owner } ->
                    Maybe.map ((,) owner) card.anim

                Nothing ->
                    Nothing

        otherwise ->
            Nothing
