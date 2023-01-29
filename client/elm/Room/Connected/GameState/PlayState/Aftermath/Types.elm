module Aftermath.Types exposing (Aftermath(..), Model)

import Leaderboard.Types as Leaderboard
import Quest.Types exposing (Quest)
import RuneSelect.Types exposing (Rune)


type alias Model =
    { aftermath : List Aftermath
    , tick : Float
    , autoRematched : Bool
    }


type Aftermath
    = Winner
    | StatChange { initialXp : Float, finalXp : Float } Float
    | Unlock Rune
    | QuestComplete Quest
    | Leaderboard Leaderboard.Leaderboard
