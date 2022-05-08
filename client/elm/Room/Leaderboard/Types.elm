module Leaderboard.Types exposing (Entry, Leaderboard, Model)


type alias Model =
    { entries : Maybe Leaderboard
    , error : String
    }


type alias Leaderboard =
    List Entry


type alias Entry =
    { name : String
    , xp : Float
    , level : Int
    , isMe : Bool
    }
