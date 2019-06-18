module Audio.State exposing (fetchSounds, playSound, playSoundWith, setVolume)

import Audio.Types exposing (SoundOption(..))
import Ports


playSound : String -> Cmd msg
playSound name =
    playSoundWith name []


playSoundWith : String -> List SoundOption -> Cmd msg
playSoundWith name options =
    let
        once : Bool
        once =
            List.member Once options

        loop : Bool
        loop =
            List.member Loop options

        vol : Float
        vol =
            let
                isVolOption : SoundOption -> Bool
                isVolOption op =
                    case op of
                        Volume _ ->
                            True

                        _ ->
                            False
            in
            case List.filter isVolOption options of
                [ Volume v ] ->
                    v

                _ ->
                    1.0
    in
    Ports.playAudio { name = name, once = once, loop = loop, vol = vol }


setVolume : Int -> Cmd msg
setVolume =
    Ports.volume


fetchSounds : List (Cmd msg)
fetchSounds =
    let
        loader : String -> Cmd msg
        loader soundPath =
            Ports.loadAudio <| "/sfx/" ++ soundPath
    in
    List.map loader sounds


sounds : List String
sounds =
    [ "draw.mp3"
    , "damage.mp3"
    , "heal.mp3"
    , "bounce.mp3"
    , "bite.mp3"
    , "curse.mp3"
    , "reverse.mp3"
    , "playCard.mp3"
    , "transmute.mp3"
    , "obliterate.mp3"
    , "burn.mp3"
    , "evilTick.mp3"
    , "victory.mp3"
    , "defeat.mp3"
    , "endTurn.mp3"
    , "hover.mp3"
    ]
