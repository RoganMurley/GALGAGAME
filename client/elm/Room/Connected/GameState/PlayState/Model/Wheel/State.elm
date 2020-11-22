module Wheel.State exposing (apply, fwrd, get, indexed, init, map, toList)

import Wheel.Types exposing (Wheel)


init : (Int -> a) -> Wheel a
init f =
    { wheel0 = f 0
    , wheel1 = f 1
    , wheel2 = f 2
    , wheel3 = f 3
    , wheel4 = f 4
    , wheel5 = f 5
    , wheel6 = f 6
    , wheel7 = f 7
    , wheel8 = f 8
    , wheel9 = f 9
    , wheel10 = f 10
    , wheel11 = f 11
    }


indexed : Wheel Int
indexed =
    init identity


toList : Wheel a -> List a
toList w =
    [ w.wheel0
    , w.wheel1
    , w.wheel2
    , w.wheel3
    , w.wheel4
    , w.wheel5
    , w.wheel6
    , w.wheel7
    , w.wheel8
    , w.wheel9
    , w.wheel10
    , w.wheel11
    ]


map : (a -> b) -> Wheel a -> Wheel b
map f x =
    { wheel0 = f x.wheel0
    , wheel1 = f x.wheel1
    , wheel2 = f x.wheel2
    , wheel3 = f x.wheel3
    , wheel4 = f x.wheel4
    , wheel5 = f x.wheel5
    , wheel6 = f x.wheel6
    , wheel7 = f x.wheel7
    , wheel8 = f x.wheel8
    , wheel9 = f x.wheel9
    , wheel10 = f x.wheel10
    , wheel11 = f x.wheel11
    }


apply : Wheel (a -> b) -> Wheel a -> Wheel b
apply f x =
    { wheel0 = f.wheel0 x.wheel0
    , wheel1 = f.wheel1 x.wheel1
    , wheel2 = f.wheel2 x.wheel2
    , wheel3 = f.wheel3 x.wheel3
    , wheel4 = f.wheel4 x.wheel4
    , wheel5 = f.wheel5 x.wheel5
    , wheel6 = f.wheel6 x.wheel6
    , wheel7 = f.wheel7 x.wheel7
    , wheel8 = f.wheel8 x.wheel8
    , wheel9 = f.wheel9 x.wheel9
    , wheel10 = f.wheel10 x.wheel10
    , wheel11 = f.wheel11 x.wheel11
    }


fwrd : Wheel a -> Wheel a
fwrd x =
    { wheel0 = x.wheel11
    , wheel1 = x.wheel0
    , wheel2 = x.wheel1
    , wheel3 = x.wheel2
    , wheel4 = x.wheel3
    , wheel5 = x.wheel4
    , wheel6 = x.wheel5
    , wheel7 = x.wheel6
    , wheel8 = x.wheel7
    , wheel9 = x.wheel8
    , wheel10 = x.wheel9
    , wheel11 = x.wheel10
    }


get : Int -> Wheel a -> Maybe a
get i wheel =
    case i of
        0 ->
            Just wheel.wheel0

        1 ->
            Just wheel.wheel1

        2 ->
            Just wheel.wheel2

        3 ->
            Just wheel.wheel3

        4 ->
            Just wheel.wheel4

        5 ->
            Just wheel.wheel5

        6 ->
            Just wheel.wheel6

        7 ->
            Just wheel.wheel7

        8 ->
            Just wheel.wheel8

        9 ->
            Just wheel.wheel9

        10 ->
            Just wheel.wheel10

        11 ->
            Just wheel.wheel11

        _ ->
            Nothing
