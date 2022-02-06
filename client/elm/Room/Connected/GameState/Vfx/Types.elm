module Vfx.Types exposing (Model)


type alias Model =
    { depth : Float
    , boogie : Float
    , tickle : Maybe ( Int, Float )
    }
