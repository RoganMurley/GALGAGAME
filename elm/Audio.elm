module Audio exposing (..)

import Ports exposing (playAudio)
import Main.Messages exposing (Msg)


type SoundOption
    = Once
    | Loop
    | Volume Float


playSound : String -> Cmd Msg
playSound name =
    playSoundWith name []


playSoundWith : String -> List SoundOption -> Cmd Msg
playSoundWith name options =
    let
        once : Bool
        once =
            List.member Once options

        loop : Bool
        loop =
            List.member Loop options

        volume : Float
        volume =
            let
                isVolOption : SoundOption -> Bool
                isVolOption op =
                    case op of
                        Volume _ ->
                            True

                        otherwise ->
                            False
            in
                case List.filter isVolOption options of
                    [ Volume v ] ->
                        v

                    otherwise ->
                        1.0
    in
        playAudio ( name, once, loop, volume )
