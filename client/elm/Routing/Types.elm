module Routing.Types exposing (..)


type Route
    = Home
    | Play PlayRoute
    | Spec String
    | Lab
    | Login


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
