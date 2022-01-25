module Routing.Types exposing (PlayRoute(..), Route(..))


type Route
    = Play PlayRoute
    | Spec String
    | Replay String
    | Login
    | Signup
    | Feedback
    | League


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
