module Audio.State exposing (fetch, playSound, playSoundWith, setVolume)

import Audio.Types exposing (SoundOption(..))
import Dict
import Manifest.Types exposing (Manifest)
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


fetch : Manifest -> List (Cmd msg)
fetch manifest =
    let
        loader : String -> Cmd msg
        loader soundPath =
            Ports.loadAudio soundPath

        revise : String -> String
        revise path =
            (Dict.get (String.dropLeft 1 path) manifest
                |> Maybe.map (\s -> "/" ++ s)
            )
                |> Maybe.withDefault path
    in
    List.map loader <| List.map revise sounds


sounds : List String
sounds =
    [ "/sfx/draw.mp3"
    , "/sfx/damage.mp3"
    , "/sfx/heal.mp3"
    , "/sfx/bounce.mp3"
    , "/sfx/bite.mp3"
    , "/sfx/curse.mp3"
    , "/sfx/reverse.mp3"
    , "/sfx/playCard.mp3"
    , "/sfx/transmuteCard.mp3"
    , "/sfx/transmuteOwner.mp3"
    , "/sfx/obliterate.mp3"
    , "/sfx/burn.mp3"
    , "/sfx/evilTick.mp3"
    , "/sfx/victory.mp3"
    , "/sfx/defeat.mp3"
    , "/sfx/endTurn.mp3"
    , "/sfx/hover.mp3"
    ]
