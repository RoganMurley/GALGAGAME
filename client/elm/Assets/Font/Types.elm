module Font.Types exposing (Entity, Font, FontChar, FontPath, Line, Model)

import Dict exposing (Dict)
import Math.Vector3 exposing (Vec3)


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
    , char : Char
    }


type alias Line =
    List FontChar


type alias FontPath =
    { name : String
    , jsonPath : String
    , texturePath : String
    }


type alias Entity =
    { x : Float
    , y : Float
    , scaleX : Float
    , scaleY : Float
    , color : Vec3
    }
