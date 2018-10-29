module Colour exposing (Colour, black, blue, card, focusBackground, green, red, white)

import Math.Vector3 exposing (Vec3, vec3)
import WhichPlayer.Types exposing (WhichPlayer(..))


type alias Colour =
    Vec3


white : Colour
white =
    vec3 0.91 0.95 0.87


black : Colour
black =
    vec3 1 1 1


red : Colour
red =
    vec3 0.9 0.2 0.0


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
            vec3 0.95 0.27 0.29

        PlayerB ->
            vec3 0.1 0.5 0.6


focusBackground : WhichPlayer -> Colour
focusBackground which =
    case which of
        PlayerA ->
            vec3 0.7 0.2 0.2

        PlayerB ->
            vec3 0.09 0.24 0.31
