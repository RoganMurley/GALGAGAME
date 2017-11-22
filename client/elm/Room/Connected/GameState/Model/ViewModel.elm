module Model.ViewModel exposing (..)


type alias ViewModel =
    { hover : Maybe Int
    , shake : Float
    }


init : ViewModel
init =
    { hover = Nothing
    , shake = 0.0
    }


shakeDecay : ViewModel -> ViewModel
shakeDecay vm =
    let
        newShake : Float
        newShake =
            if vm.shake < 0.1 then
                0
            else
                vm.shake * 0.9
    in
        { vm | shake = newShake }
