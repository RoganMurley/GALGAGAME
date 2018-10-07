module Audio exposing (..)

import Ports exposing (playAudio, volume)


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
        playAudio ( name, once, loop, vol )


setVolume : Int -> Cmd msg
setVolume vol =
    volume vol
