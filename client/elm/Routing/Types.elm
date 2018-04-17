module Routing.Types exposing (..)


type Route
    = Home
    | Play PlayRoute
    | Spec String
    | Replay String
    | Lab
    | Login
    | Clock


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
