-- Based on https:://github.com/elm-community/easing-functions/blob/2.0.0/src/Ease.elm
module Ease where

inToOut :: (Float -> Float) -> (Float -> Float)
inToOut f x = 1 - f (1 - x)

linear :: Float -> Float
linear = id

inQuad :: Float -> Float
inQuad x = x ** 2

inCubic :: Float -> Float
inCubic x = x ** 3

inQuart :: Float -> Float
inQuart x = x ** 4

inQuint :: Float -> Float
inQuint x = x ** 5

outQuad :: Float -> Float
outQuad = inToOut inQuad

outCubic :: Float -> Float
outCubic = inToOut inCubic

outQuart :: Float -> Float
outQuart = inToOut inQuart

outQuint :: Float -> Float
outQuint = inToOut inQuint
