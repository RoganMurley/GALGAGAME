module World.Types exposing (Decision, DecisionChoice, Encounter, Model, Variant(..), World)

import Buttons.Types exposing (Buttons(..))
import Card.Types exposing (Card)
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
    , waitPvp : Maybe ( Float, Int )
    }


type alias Encounter =
    { guid : String
    , x : Float
    , y : Float
    , variant : Variant
    }


type Variant
    = CpuVariant
    | PvpVariant


type alias Decision =
    { id : String
    , title : String
    , text : String
    , cards : List Card
    , choices : List DecisionChoice
    }


type alias DecisionChoice =
    { text : String
    }
