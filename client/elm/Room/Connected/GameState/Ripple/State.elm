module Ripple.State exposing (add, tick)

import Math.Vector2 exposing (Vec2)
import Maybe.Extra as Maybe
import Ripple.Types exposing (Ripple)


add : Vec2 -> { a | ripples : List Ripple } -> { a | ripples : List Ripple }
add pos model =
    { model | ripples = { progress = 1000, pos = pos } :: model.ripples }


tick : List Ripple -> Float -> List Ripple
tick ripples dt =
    Maybe.values <|
        List.map
            (\ripple ->
                let
                    progress =
                        ripple.progress - dt
                in
                if progress < 0 then
                    Nothing

                else
                    Just { ripple | progress = progress }
            )
            ripples
