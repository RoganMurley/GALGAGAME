module Transform exposing (..)

import Ease


type alias Transform =
    { x : Float
    , y : Float
    , r : Float
    }


ease : Ease.Easing -> Float -> Transform -> Transform -> Transform
ease easing t start final =
    let
        diff : Transform
        diff =
            { x = final.x - start.x
            , y = final.y - start.y
            , r = final.r - start.r
            }
    in
        { x = start.x + (easing t) * diff.x
        , y = start.y + (easing t) * diff.y
        , r = start.r + (easing t) * diff.r
        }


toCss : Transform -> ( String, String )
toCss { x, y, r } =
    ( "transform"
    , "translate("
        ++ toString x
        ++ "rem, "
        ++ toString y
        ++ "rem) rotate("
        ++ toString r
        ++ "deg)"
    )


origin : Transform
origin =
    { x = 0, y = 0, r = 0 }
