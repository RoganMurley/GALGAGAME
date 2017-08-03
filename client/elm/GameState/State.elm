module GameState.State exposing (resTick, update, tickForward, tickZero)

import CharacterSelect.State as CharacterSelect
import CharacterSelect.Types as CharacterSelect
import GameState.Decoders exposing (decodeState, resDecoder)
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..))
import Json.Decode as Json exposing (field, maybe)
import Main.Messages as Main
import Model.Types exposing (..)
import Util exposing (fromJust, message, safeTail)


update : Msg -> GameState -> ( GameState, Cmd Main.Msg )
update msg state =
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
                                ( state, message <| Main.GameStateMsg <| msg )

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
                                ( PlayingGame ( oldModel, oldVm ) ( resList ++ [ newModel ], 0 )
                                , Cmd.none
                                )

                            ( PlayingGame ( oldModel, oldVm ) _, Ended w _ _ ) ->
                                ( Ended w (Just oldModel) ( resList, 0 )
                                , Cmd.none
                                )

                            otherwise ->
                                ( setRes final resList
                                , Cmd.none
                                )

        SelectingMsg selectMsg ->
            let
                model : CharacterSelect.Model
                model =
                    case state of
                        Selecting m ->
                            m

                        otherwise ->
                            Debug.crash "Expected a selecting state"

                ( newModel, cmd ) =
                    CharacterSelect.update selectMsg model
            in
                ( Selecting newModel, cmd )


setRes : GameState -> List Model -> GameState
setRes state res =
    case state of
        PlayingGame ( m, vm ) ( _, i ) ->
            PlayingGame ( m, vm ) ( res, i )

        Ended w m ( _, i ) ->
            Ended w m ( res, i )

        Waiting ->
            Debug.crash "Set res on a waiting state"

        Selecting _ ->
            Debug.crash "Set res on a Selecting state"


syncState : GameState -> String -> GameState
syncState oldState msg =
    carryVm oldState (decodeState msg oldState)



-- Carry the old viewmodel between a new and olf GameState


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


resDelay : Int
resDelay =
    35


resTick : GameState -> GameState
resTick state =
    case state of
        PlayingGame ( model, vm ) ( res, _ ) ->
            case List.head res of
                Just newModel ->
                    PlayingGame ( newModel, vm ) ( safeTail res, resDelay )

                Nothing ->
                    PlayingGame ( model, vm ) ( res, 0 )

        Ended which (Just model) ( res, _ ) ->
            Ended
                which
                (List.head res)
                ( List.drop 1 res, resDelay )

        otherwise ->
            state


tickForward : GameState -> GameState
tickForward state =
    case state of
        PlayingGame model ( res, tick ) ->
            PlayingGame model ( res, tick - 1 )

        Ended which model ( res, tick ) ->
            Ended which model ( res, tick - 1 )

        otherwise ->
            state


tickZero : GameState -> Bool
tickZero state =
    case state of
        PlayingGame _ ( _, 0 ) ->
            True

        Ended _ _ ( _, 0 ) ->
            True

        otherwise ->
            False
