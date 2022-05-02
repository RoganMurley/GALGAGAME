module Leaderboard.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, list, string)
import Leaderboard.Types exposing (Entry)
import Stats exposing (levelFromExperience)


decoder : Decoder (List Entry)
decoder =
    let
        makeEntry : String -> Float -> Entry
        makeEntry name xp =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            }
    in
    list <|
        Json.map2 makeEntry
            (field "name" string)
            (field "xp" float)
