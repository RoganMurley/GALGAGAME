module Lab.Types exposing (Model)

import Card.Types exposing (Anim)
import Model.Types exposing (WhichPlayer)


type alias Model =
    { player : WhichPlayer
    , anim : Anim
    , time : Float
    }
