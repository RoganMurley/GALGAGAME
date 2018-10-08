module Colour exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import WhichPlayer.Types exposing (WhichPlayer(..))


type alias Colour =
    Vec3


white : Colour
white =
    vec3 1 1 1


black : Colour
black =
    vec3 1 1 1


red : Colour
red =
    vec3 1 0 0


green : Colour
green =
    vec3 0 1 0


blue : Colour
blue =
    vec3 0 0 1


card : WhichPlayer -> Colour
card which =
    case which of
        PlayerA ->
            vec3 0.52 0.1 0.2

        PlayerB ->
            vec3 0.18 0.49 0.62


focusBackground : WhichPlayer -> Colour
focusBackground which =
    case which of
        PlayerA ->
            vec3 0.26 0.05 0.1

        PlayerB ->
            vec3 0.09 0.24 0.31
