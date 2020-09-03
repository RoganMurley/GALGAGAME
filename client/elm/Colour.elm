module Colour exposing (Colour, background, black, blue, card, cardCol, darkGray, focusBackground, glyph, green, red, white, yellow)

import Card.Types exposing (CardCol(..))
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


cardCol : CardCol -> Colour
cardCol col =
    case col of
        Red ->
            vec3 (255 / 255) (0 / 255) (0 / 255)

        Orange ->
            vec3 (255 / 255) (127 / 255) (0 / 255)

        Yellow ->
            vec3 (255 / 255) (255 / 255) (0 / 255)

        Green ->
            vec3 (0 / 255) (255 / 255) (0 / 255)

        Blue ->
            vec3 (0 / 255) (0 / 255) (255 / 255)

        White ->
            vec3 (225 / 255) (225 / 255) (225 / 255)

        Violet ->
            vec3 (139 / 255) (0 / 255) (255 / 255)

        Copper ->
            vec3 (166 / 255) (25 / 255) (27 / 255)

        Mystery ->
            vec3 (255 / 255) (255 / 255) (255 / 255)
