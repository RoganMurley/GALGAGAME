module World.Types exposing (Encounter, Model, World)

import Buttons.Types exposing (Buttons(..))


type alias Model =
    { buttons : Buttons
    , time : Float
    , world : World
    }


type alias World =
    List Encounter


type alias Encounter =
    { guid : String
    , name : String
    , x : Float
    , y : Float
    }
