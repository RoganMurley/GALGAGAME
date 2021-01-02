module World.Types exposing (Encounter, Model, World)

import Buttons.Types exposing (Buttons(..))


type alias Model =
    { buttons : Buttons
    , time : Float
    , world : World
    }


type alias World =
    { encounters : List Encounter
    , others : List ( Float, Float )
    }


type alias Encounter =
    { guid : String
    , name : String
    , numeral : String
    , x : Float
    , y : Float
    }
