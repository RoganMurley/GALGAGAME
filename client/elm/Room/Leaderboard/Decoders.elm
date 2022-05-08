module Leaderboard.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, bool, field, float, list, string)
import Leaderboard.Types exposing (Entry)
import Stats exposing (levelFromExperience)


decoder : Decoder (List Entry)
decoder =
    let
        makeEntry : String -> Float -> Bool -> Entry
        makeEntry name xp isMe =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            , isMe = isMe
            }
    in
    field "leaderboard" <|
        list <|
            Json.map3 makeEntry
                (field "name" string)
                (field "xp" float)
                (field "is_me" bool)
