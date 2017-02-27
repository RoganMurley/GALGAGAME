port module Ports exposing (..)


port queryParams : String -> Cmd msg


port selectAllInput : String -> Cmd msg


port copyInput : String -> Cmd msg


port playAudio : ( String, Bool, Bool, Float ) -> Cmd msg
