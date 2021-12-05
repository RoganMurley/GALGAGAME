port module Ports exposing (analytics, copyInput, godModeCommand, loadAudio, log, mouseDown, mouseMove, mouseUp, playAudio, reload, scaling, selectAllInput, setTitle, touch, volume, websocketListen, websocketReconnect, websocketSend)

import Audio.Types exposing (PlayAudioMessage)
import Mouse


port selectAllInput : String -> Cmd msg


port copyInput : String -> Cmd msg


port loadAudio : String -> Cmd msg


port playAudio : PlayAudioMessage -> Cmd msg


port volume : Int -> Cmd msg


port scaling : Float -> Cmd msg


port log : String -> Cmd msg


port reload : () -> Cmd msg


port analytics : () -> Cmd msg


port websocketReconnect : () -> Cmd msg


port websocketSend : String -> Cmd msg


port setTitle : String -> Cmd msg


port mouseDown : (Mouse.Position -> msg) -> Sub msg


port mouseUp : (Mouse.Position -> msg) -> Sub msg


port mouseMove : (Mouse.Position -> msg) -> Sub msg


port touch : (Maybe Mouse.Position -> msg) -> Sub msg


port godModeCommand : (String -> msg) -> Sub msg


port websocketListen : (String -> msg) -> Sub msg
