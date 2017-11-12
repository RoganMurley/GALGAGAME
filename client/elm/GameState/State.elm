module GameState.State exposing (activeAnim, resTick, update, tickForward, tickZero, gameTickStart)

import Card.Types exposing (Anim)
import CharacterSelect.State as CharacterSelect
import GameState.Decoders exposing (decodeState, resDecoder)
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..))
import Json.Decode as Json exposing (field, maybe)
import Main.Messages as Main
import Model.Types exposing (..)
import ViewModel.State as ViewModel
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
                            CharacterSelect.update selectMsg m
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

                s ->
                    ( s, Cmd.none )


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


tickForward : GameState -> Float -> GameState
tickForward game dt =
    case game of
        PlayingGame ( m, vm ) ( res, tick ) ->
            if tickZero tick then
                resTick game
            else
                PlayingGame ( m, (ViewModel.shakeDecay vm) ) ( res, tick + dt )

        Ended which final vm resModel ( res, tick ) ->
            if tickZero tick then
                resTick game
            else
                Ended which final (ViewModel.shakeDecay vm) resModel ( res, tick + dt )

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
