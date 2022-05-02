module Leaderboard.Types exposing (Entry, Model)


type alias Model =
    { entries : List Entry
    , error : String
    }


type alias Entry =
    { name : String
    , xp : Float
    , level : Int
    }
