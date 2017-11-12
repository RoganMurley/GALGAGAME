module Compass.Types exposing (..)


type Route
    = Home
    | Play PlayRoute
    | Lab


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
