module Listener exposing (animSfx, listen)

import Animation.Types exposing (Anim(..), Hurt(..))
import Audio.State exposing (playSoundWith)
import Audio.Types as Audio exposing (SoundOption(..))
import GameState.Types exposing (GameState(..))
import Main.Messages exposing (Msg)
import PlayState.State as PlayState
import Resolvable.State exposing (activeAnim, tickStart)
import Resolvable.Types as Resolvable
import WhichPlayer.Types exposing (WhichPlayer(..))


listen : Audio.Model -> GameState -> Float -> Cmd Msg
listen sounds state tick =
    let
        modelListen : Resolvable.Model -> Cmd Msg
        modelListen res =
            if tickStart res then
                case animSfx <| activeAnim res of
                    Just url ->
                        playSoundWith
                            sounds
                            ("sfx/" ++ url)
                            [ Volume 0.5 ]

                    Nothing ->
                        Cmd.none

            else
                Cmd.none
    in
    if tick == 0 then
        playSoundWith sounds "music/background.mp3" [ Loop, Once ]

    else
        case state of
            Started playstate ->
                modelListen <| PlayState.get .res playstate

            _ ->
                Cmd.none


animSfx : Anim -> Maybe String
animSfx anim =
    case anim of
        Hurt _ d hurt ->
            case d of
                0 ->
                    Nothing

                _ ->
                    case hurt of
                        Slash ->
                            Just "damage.mp3"

                        Bite ->
                            Just "bite.mp3"

                        Curse ->
                            Just "curse.mp3"

        Heal _ h ->
            if h > 0 then
                Just "heal.mp3"

            else
                Nothing

        Draw _ _ ->
            Just "draw.mp3"

        Play _ _ _ _ ->
            Just "slash.mp3"

        Transmute _ ->
            Just "transmute.mp3"

        DiscardStack _ ->
            Just "obliterate.mp3"

        DiscardHand _ _ ->
            Just "obliterate.mp3"

        Mill _ _ _ ->
            Just "burn.mp3"

        GameEnd winner ->
            case winner of
                Just PlayerA ->
                    Just "victory.mp3"

                Just PlayerB ->
                    Just "defeat.mp3"

                Nothing ->
                    Just "draw.mp3"

        HandFullPass ->
            Just "evilTick.mp3"

        Bounce _ ->
            Just "bounce.mp3"

        Rotate _ ->
            Just "rotate.mp3"

        Windup _ ->
            Just "windup.mp3"

        MoveStack _ _ ->
            Just "moveStack.mp3"

        _ ->
            Nothing
