module Transform exposing (..)

import Ease


type alias Transform =
    { x : Float
    , y : Float
    , r : Float
    , sx : Float
    , sy : Float
    }


ease : Ease.Easing -> Float -> Transform -> Transform -> Transform
ease easing t start final =
    let
        diff : Transform
        diff =
            { x = final.x - start.x
            , y = final.y - start.y
            , r = final.r - start.r
            , sx = final.sx - start.sx
            , sy = final.sy - start.sy
            }
    in
        { x = start.x + (easing t) * diff.x
        , y = start.y + (easing t) * diff.y
        , r = start.r + (easing t) * diff.r
        , sx = start.sx + (easing t) * diff.sx
        , sy = start.sy + (easing t) * diff.sy
        }


toCss : Transform -> ( String, String )
toCss { x, y, r, sx, sy } =
    ( "transform"
    , "translate("
        ++ toString x
        ++ "rem, "
        ++ toString y
        ++ "rem) rotate("
        ++ toString r
        ++ "deg) skew("
        ++ toString sx
        ++ "deg, "
        ++ toString sy
        ++ "deg)"
    )


origin : Transform
origin =
    { x = 0, y = 0, r = 0, sx = 0, sy = 0 }
