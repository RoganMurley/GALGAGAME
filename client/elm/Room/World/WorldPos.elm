module World.WorldPos exposing (lineToWorldPos, toWorldPos)

import Game.Types exposing (Context)
import Line.Types exposing (Line)


toWorldPos : Context -> { a | x : Float, y : Float } -> { x : Float, y : Float }
toWorldPos { w, h, radius } { x, y } =
    { x = 0.5 * w + ((x - 0.5) * radius * 5)
    , y = 0.5 * h + ((y - 0.5) * radius * 5)
    }


lineToWorldPos : Context -> Line -> Line
lineToWorldPos ctx line =
    let
        start =
            toWorldPos ctx { x = line.x1, y = line.y1 }

        end =
            toWorldPos ctx { x = line.x2, y = line.y2 }
    in
    { x1 = start.x
    , y1 = start.y
    , x2 = end.x
    , y2 = end.y
    }
