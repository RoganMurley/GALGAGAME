module DeckBuilding.Types exposing (Character, Model, Rune, RuneCards)

import Card.Types exposing (Card)


type alias Model =
    { characters : List Character
    , index : Int
    , ready : Bool
    }


type alias Character =
    { name : String
    , rune_a : Rune
    , rune_b : Rune
    , rune_c : Rune
    }


type alias Rune =
    { name : String
    , imgURL : String
    , cards : RuneCards
    }


type alias RuneCards =
    { a : Card
    , b : Card
    , c : Card
    , d : Card
    }
