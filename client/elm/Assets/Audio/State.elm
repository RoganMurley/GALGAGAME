module Audio.State exposing (fetch, init, playSound, playSoundWith, setVolume, update)

import Assets.Messages as Assets
import Audio.Messages exposing (Msg(..))
import Audio.Types exposing (Model, SoundOption(..))
import Dict exposing (Dict)
import Main.Messages as Main
import Manifest.Types exposing (Manifest)
import Ports
import Util exposing (message)


init : Model
init =
    { sounds = Dict.empty }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SoundsLoaded sounds ->
            { sounds = Dict.union sounds model.sounds }


playSound : Model -> String -> Cmd msg
playSound model name =
    playSoundWith model name []


playSoundWith : Model -> String -> List SoundOption -> Cmd msg
playSoundWith { sounds } name options =
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

        revName =
            Dict.get name sounds |> Maybe.withDefault name
    in
    Ports.playAudio { name = revName, once = once, loop = loop, vol = vol }


setVolume : Int -> Cmd msg
setVolume =
    Ports.volume


fetch : Manifest -> List (Cmd Main.Msg)
fetch manifest =
    let
        loader : String -> Cmd msg
        loader soundPath =
            Ports.loadAudio soundPath

        sounds : Dict String String
        sounds =
            Dict.filter
                (\key _ ->
                    String.startsWith "sfx/" key
                        || String.startsWith "music/" key
                )
                manifest

        manifestLoadedCmd : Cmd Main.Msg
        manifestLoadedCmd =
            message <|
                Main.AssetsMsg <|
                    Assets.AudioMsg <|
                        SoundsLoaded sounds
    in
    manifestLoadedCmd :: List.map loader (Dict.values sounds)
