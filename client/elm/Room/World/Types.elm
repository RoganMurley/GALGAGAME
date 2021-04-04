module World.Types exposing (Decision, DecisionChoice, Encounter, Model, World)

import Buttons.Types exposing (Buttons(..))
import Line.Types exposing (Line)


type alias Model =
    { encounterButtons : Buttons
    , otherButtons : Buttons
    , visitedButtons : Buttons
    , choiceButtons : Buttons
    , time : Float
    , world : World
    }


type alias World =
    { encounters : List Encounter
    , others : List ( Float, Float )
    , edges : List Line
    , visited : List ( Float, Float )
    , visitedEdges : List Line
    , lockedEdges : List Line
    , decision : Maybe Decision
    }


type alias Encounter =
    { guid : String
    , name : String
    , x : Float
    , y : Float
    }


type alias Decision =
    { id : String
    , title : String
    , text : String
    , choices : List DecisionChoice
    }


type alias DecisionChoice =
    { text : String
    }
