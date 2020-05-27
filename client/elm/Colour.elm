module Colour exposing (Colour, background, black, blue, card, darkGray, focusBackground, glyph, green, nettle, red, tea, teaRed, white, yellow)

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


nettle : Colour
nettle =
    vec3 0.679 0.755 0.604


card : WhichPlayer -> Colour
card which =
    case which of
        PlayerA ->
            vec3 0.95 0.27 0.29

        PlayerB ->
            vec3 0.14 0.52 0.75


background : WhichPlayer -> Colour
background which =
    case which of
        PlayerA ->
            vec3 0.75 0.07 0.09

        PlayerB ->
            vec3 0.0 0.32 0.55


focusBackground : WhichPlayer -> Colour
focusBackground which =
    case which of
        PlayerA ->
            vec3 0.95 0.27 0.29

        PlayerB ->
            vec3 0.14 0.52 0.75


darkGray : Colour
darkGray =
    vec3 0.15 0.15 0.15


glyph : WhichPlayer -> Colour
glyph which =
    case which of
        PlayerA ->
            tea

        PlayerB ->
            teaRed


tea : Colour
tea =
    vec3 0.26 0.13 0.04


teaRed : Colour
teaRed =
    vec3 0.75 0 0
