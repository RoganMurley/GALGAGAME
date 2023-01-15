module Routing.Types exposing (PlayRoute(..), Route(..))


type Route
    = Play PlayRoute
    | Spec String
    | Replay String
    | Login
    | Signup
    | Feedback
    | League
    | Leaderboard
    | Profile String
    | Create


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay (Maybe String)
    | QuickPlay (Maybe String)
