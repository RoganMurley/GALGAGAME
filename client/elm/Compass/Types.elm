module Compass.Types exposing (..)


type Route
    = Home
    | Play PlayRoute


type PlayRoute
    = CustomPlay (Maybe String)
    | ComputerPlay
    | QuickPlay
