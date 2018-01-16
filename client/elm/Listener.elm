module Listener exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import GameState.State as GameState exposing (resolvable)
import GameState.Types exposing (GameState(..))
import Model.Types exposing (Model)
import Main.Messages exposing (Msg)
import Resolvable.State exposing (activeModel, activeStackCard)


listen : Float -> GameState -> Cmd Msg
listen time state =
    let
        modelListen : GameState -> Model -> Cmd Msg
        modelListen state m =
            if GameState.gameTickStart state then
                let
                    sfxURL : Maybe String
                    sfxURL =
                        case state of
                            Started started ->
                                case activeStackCard <| resolvable started of
                                    Just { card } ->
                                        Just card.sfxURL

                                    Nothing ->
                                        Nothing

                            otherwise ->
                                Nothing

                    volume : Float
                    volume =
                        0.5 + 0.1 * (toFloat (List.length m.stack))
                in
                    case sfxURL of
                        Just url ->
                            playSoundWith ("/sfx/" ++ url) [ Volume volume ]

                        Nothing ->
                            Cmd.none
            else
                Cmd.none
    in
        case state of
            Selecting _ ->
                playSoundWith "/music/select.mp3" [ Loop, Once ]

            Started started ->
                modelListen state <|
                    activeModel <|
                        resolvable started

            otherwise ->
                Cmd.none
