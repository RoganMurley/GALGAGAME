module Aftermath.Types exposing (Aftermath(..), Model)

import Stats exposing (StatChange)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { aftermath : List Aftermath
    , tick : Float
    }


type Aftermath
    = Winner
    | StatChange StatChange Float
