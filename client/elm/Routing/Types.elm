module Routing.Types exposing (PlayRoute(..), Route(..))


type Route
    = Home
    | Play PlayRoute
    | Spec String
    | Replay String
    | Login
    | Signup


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
