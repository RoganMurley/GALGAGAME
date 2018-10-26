port module Ports exposing (analytics, copyInput, godModeCommand, playAudio, reload, selectAllInput, touch, volume)


port selectAllInput : String -> Cmd msg


port copyInput : String -> Cmd msg


port playAudio : ( String, Bool, Bool, Float ) -> Cmd msg


port volume : Int -> Cmd msg


port reload : () -> Cmd msg


port analytics : () -> Cmd msg


port touch : ({ x : Int, y : Int } -> msg) -> Sub msg


port godModeCommand : (String -> msg) -> Sub msg
