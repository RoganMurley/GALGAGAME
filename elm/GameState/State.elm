module GameState.State exposing (resTick, update, tickForward, tickZero)

import CharacterSelect.State as CharacterSelect
import CharacterSelect.Types as CharacterSelect
import GameState.Decoders exposing (decodeState, resDecoder)
import GameState.Messages as GameState
import GameState.Types exposing (GameState(..))
import Json.Decode as Json exposing (field, maybe)
import Model.Types exposing (..)
import Util exposing (fromJust, safeTail)


update : GameState.Msg -> GameState -> GameState
update msg state =
    case msg of
        GameState.Sync str ->
            syncState state str

        GameState.HoverOutcome i ->
            case state of
                PlayingGame m r ->
                    PlayingGame { m | otherHover = i } r

                s ->
                    s

        GameState.ResolveOutcome str ->
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
                        setRes final []

                    otherwise ->
                        case ( state, final ) of
                            ( PlayingGame oldModel _, PlayingGame newModel _ ) ->
                                PlayingGame oldModel ( resList ++ [ newModel ], 0 )

                            ( PlayingGame oldModel _, Ended w _ _ ) ->
                                Ended w (Just oldModel) ( resList, 0 )

                            otherwise ->
                                setRes final resList

        GameState.SelectingMsg selectMsg ->
            let
                model : CharacterSelect.Model
                model =
                    case state of
                        Selecting m ->
                            m

                        otherwise ->
                            Debug.crash "Expected a selecting state"
            in
                Selecting (CharacterSelect.update selectMsg model)


setRes : GameState -> List Model -> GameState
setRes state res =
    case state of
        PlayingGame m ( _, i ) ->
            PlayingGame m ( res, i )

        Ended w m ( _, i ) ->
            Ended w m ( res, i )

        Waiting ->
            Debug.crash "Set res on a waiting state"

        Selecting _ ->
            Debug.crash "Set res on a Selecting state"


syncState : GameState -> String -> GameState
syncState oldState msg =
    decodeState msg oldState


resDelay : Int
resDelay =
    35


resTick : GameState -> GameState
resTick state =
    case state of
        PlayingGame model ( res, _ ) ->
            case List.head res of
                Just newModel ->
                    PlayingGame newModel ( safeTail res, resDelay )

                Nothing ->
                    PlayingGame model ( res, 0 )

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
