module Routing.Types exposing (PlayRoute(..), Route(..))


type Route
    = Play PlayRoute
    | Spec String
    | Replay String (Maybe Int)
    | Login
    | Signup
    | Feedback
    | League
    | Leaderboard
    | Profile String
    | Create
    | Entrypoint


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay (Maybe String)
    | QuickPlay (Maybe String)
    | ChallengePlay (Maybe String)
