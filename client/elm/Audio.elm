module Audio exposing (SoundOption(..), fetchSounds, playSound, playSoundWith, setVolume)

import Ports


type SoundOption
    = Once
    | Loop
    | Volume Float


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
    Ports.playAudio ( name, once, loop, vol )


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
    [ "draw.wav"
    , "damage.mp3"
    , "heal.mp3"
    , "bite.wav"
    , "reverse.mp3"
    , "playCard.wav"
    , "transmute.mp3"
    , "obliterate.mp3"
    , "burn.mp3"
    , "victory.wav"
    , "defeat.mp3"
    ]
