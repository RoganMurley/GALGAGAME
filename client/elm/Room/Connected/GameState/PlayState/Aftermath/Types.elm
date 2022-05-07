module Aftermath.Types exposing (Aftermath(..), Model)

import Leaderboard.Types as Leaderboard
import RuneSelect.Types exposing (Rune)


type alias Model =
    { aftermath : List Aftermath
    , tick : Float
    }


type Aftermath
    = Winner
    | StatChange { initialXp : Float, finalXp : Float } Float
    | Unlock Rune
    | Leaderboard Leaderboard.Leaderboard
