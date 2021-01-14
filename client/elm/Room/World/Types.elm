module World.Types exposing (Encounter, Model, World)

import Buttons.Types exposing (Buttons(..))
import Line.Types exposing (Line)


type alias Model =
    { encounterButtons : Buttons
    , otherButtons : Buttons
    , visitedButtons : Buttons
    , time : Float
    , world : World
    }


type alias World =
    { encounters : List Encounter
    , others : List ( Float, Float )
    , edges : List Line
    , visited : List ( Float, Float )
    , visitedEdges : List Line
    }


type alias Encounter =
    { guid : String
    , name : String
    , numeral : String
    , x : Float
    , y : Float
    }
