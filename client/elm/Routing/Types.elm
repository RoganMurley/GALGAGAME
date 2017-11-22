module Routing.Types exposing (..)


type Route
    = Home
    | Play PlayRoute
    | Spec String
    | Lab


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
