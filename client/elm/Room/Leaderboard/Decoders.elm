module Leaderboard.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, bool, field, float, int, list, string)
import Leaderboard.Types exposing (Entry)
import Stats exposing (levelFromExperience)


decoder : Decoder (List Entry)
decoder =
    let
        makeEntry : String -> Float -> Int -> Bool -> Entry
        makeEntry name xp rank isMe =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            , rank = rank
            , isMe = isMe
            }
    in
    field "leaderboard" <|
        list <|
            Json.map4 makeEntry
                (field "name" string)
                (field "xp" float)
                (field "rank" int)
                (field "is_me" bool)
