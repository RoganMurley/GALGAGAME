module DeckBuilding.Types exposing (Character, Characters, Model, Rune, RuneCards)

import Card.Types exposing (Card)


type alias Model =
    { characters : Characters
    , ready : Bool
    }


type alias Characters =
    { previous : List Character
    , selected : Character
    , remaining : List Character
    }


type alias Character =
    { name : String
    , imgUrl : String
    , runeA : Rune
    , runeB : Rune
    , runeC : Rune
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
