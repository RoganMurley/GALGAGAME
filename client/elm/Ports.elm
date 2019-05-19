port module Ports exposing (analytics, click, copyInput, godModeCommand, mouseMove, playAudio, reload, selectAllInput, touch, volume)

import Mouse


port selectAllInput : String -> Cmd msg


port copyInput : String -> Cmd msg


port playAudio : ( String, Bool, Bool, Float ) -> Cmd msg


port volume : Int -> Cmd msg


port reload : () -> Cmd msg


port analytics : () -> Cmd msg


port click : (Mouse.Position -> msg) -> Sub msg


port mouseMove : (Mouse.Position -> msg) -> Sub msg


port touch : (Maybe Mouse.Position -> msg) -> Sub msg


port godModeCommand : (String -> msg) -> Sub msg
