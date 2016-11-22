module Util exposing (applyFst, px)

-- Convert to pixel css representation.


px : Int -> String
px number =
    toString number ++ "px"



-- Apply a function to the first element of a typle.


applyFst : (a -> c) -> ( a, b ) -> ( c, b )
applyFst f ( x, y ) =
    ( f x, y )
