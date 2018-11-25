module Listener exposing (animSfx, listen)

import Animation.Types exposing (Anim(..))
import Audio exposing (SoundOption(..), playSoundWith)
import GameState.Types exposing (GameState(..))
import Main.Messages exposing (Msg)
import PlayState.State as PlayState
import Resolvable.State exposing (activeAnim, tickStart)
import Resolvable.Types as Resolvable
import WhichPlayer.Types exposing (WhichPlayer(..))


listen : GameState -> Cmd Msg
listen state =
    let
        modelListen : Resolvable.Model -> Cmd Msg
        modelListen res =
            if tickStart res then
                case animSfx <| activeAnim res of
                    Just url ->
                        playSoundWith
                            ("/sfx/" ++ url)
                            [ Volume 0.5 ]

                    Nothing ->
                        Cmd.none

            else
                Cmd.none
    in
    case state of
        Selecting _ ->
            playSoundWith "/music/bebopboi.mp3" [ Loop, Once ]

        Started playstate ->
            modelListen <| PlayState.get .res playstate

        _ ->
            Cmd.none


animSfx : Anim -> Maybe String
animSfx anim =
    case anim of
        Slash _ d ->
            case d of
                0 ->
                    Nothing

                _ ->
                    Just "damage.mp3"

        Heal _ _ ->
            Just "heal.mp3"

        Draw _ ->
            Just "draw.wav"

        Bite _ _ ->
            Just "bite.wav"

        Reverse _ ->
            Just "reverse.mp3"

        Play _ _ _ ->
            Just "playCard.wav"

        Transmute _ _ _ ->
            Just "transmute.mp3"

        Hubris _ ->
            Just "obliterate.mp3"

        Mill _ _ ->
            Just "burn.mp3"

        GameEnd winner ->
            case winner of
                Just PlayerA ->
                    Just "victory.wav"

                Just PlayerB ->
                    Just "defeat.mp3"

                Nothing ->
                    Just "draw.wav"

        _ ->
            Nothing
