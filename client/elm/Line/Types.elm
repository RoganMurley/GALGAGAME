module Line.Types exposing (Line, Options)

import Math.Vector3 exposing (Vec3)


type alias Line =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


type alias Options =
    { thickness : Float
    , col : Vec3
    , alpha : Float
    }
