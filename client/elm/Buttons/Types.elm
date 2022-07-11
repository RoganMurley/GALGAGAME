module Buttons.Types exposing (Button, ButtonType(..), Buttons(..), ImageButtonParams, TextButtonOption(..), TextButtonParams)

import Dict exposing (Dict)
import Math.Vector3 exposing (Vec3)


type Buttons
    = Buttons (Dict String Button)


type alias Button =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , hover : Float
    , disabled : Bool
    , btn : ButtonType
    }


type ButtonType
    = TextButton TextButtonParams
    | ImageButton ImageButtonParams


type alias TextButtonParams =
    { font : String
    , text : String
    , bgColor : Vec3
    , textColor : Vec3
    , options : List TextButtonOption
    }


type alias ImageButtonParams =
    { img : String
    , color : Vec3
    }


type TextButtonOption
    = HoverText String
    | TextScale Float
    | Circular
    | NoHover
    | IsIcon
