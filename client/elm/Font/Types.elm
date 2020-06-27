module Font.Types exposing (Font, FontChar, Model)

import Dict exposing (Dict)


type alias Model =
    { fonts : Dict String Font }


type alias Font =
    Dict Char FontChar


type alias FontChar =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , originX : Int
    , originY : Int
    , advance : Int
    }
