module Font.Types exposing (Font, FontChar, FontPath, Model)

import Dict exposing (Dict)


type alias Model =
    { fonts : Dict String Font }


type alias Font =
    Dict Char FontChar


type alias FontChar =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , originX : Float
    , originY : Float
    , advance : Float
    }


type alias FontPath =
    { name : String
    , jsonPath : String
    , texturePath : String
    }
