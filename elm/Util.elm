module Util exposing (px)


-- Convert to pixel css representation.
px : Int -> String
px number =
  toString number ++ "px"
