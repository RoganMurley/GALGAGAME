module Colour exposing (Colour, background, black, blue, card, darkGray, focusBackground, glyph, green, red, white, yellow)

import Math.Vector3 exposing (Vec3, vec3)
import WhichPlayer.Types exposing (WhichPlayer(..))


type alias Colour =
    Vec3


white : Colour
white =
    vec3 1 1 1


black : Colour
black =
    vec3 0 0 0


red : Colour
red =
    vec3 0.65 0.12 0.14


green : Colour
green =
    vec3 0.27 0.95 0.45


blue : Colour
blue =
    vec3 0 0 1


yellow : Colour
yellow =
    vec3 0.96 0.95 0.37


card : WhichPlayer -> Colour
card which =
    case which of
        PlayerA ->
            vec3 (255 / 255) (59 / 255) (59 / 255)

        PlayerB ->
            vec3 (41 / 255) (171 / 255) (226 / 255)


background : WhichPlayer -> Colour
background which =
    case which of
        PlayerA ->
            vec3 (237 / 255) (28 / 255) (36 / 255)

        PlayerB ->
            vec3 (0 / 255) (113 / 255) (188 / 255)


focusBackground : WhichPlayer -> Colour
focusBackground which =
    case which of
        PlayerA ->
            black

        PlayerB ->
            white


darkGray : Colour
darkGray =
    vec3 0.15 0.15 0.15


glyph : WhichPlayer -> Colour
glyph which =
    case which of
        PlayerA ->
            vec3 (26 / 255) (127 / 255) (138 / 255)

        PlayerB ->
            vec3 (191 / 255) (49 / 255) (14 / 255)
