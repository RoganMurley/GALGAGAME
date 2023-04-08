module Profile.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, bool, field, float, int, list, maybe, string)
import Profile.Types exposing (Profile, ProfileReplay)
import Stats exposing (levelFromExperience)


decoder : Decoder Profile
decoder =
    let
        makeProfile : String -> Int -> Float -> List ProfileReplay -> Bool -> Profile
        makeProfile name id xp replays online =
            { name = name
            , id = id
            , xp = xp
            , level = levelFromExperience xp
            , replays = replays
            , online = online
            , isMe = False
            }
    in
    Json.map5 makeProfile
        (field "name" string)
        (field "id" int)
        (field "xp" float)
        (field "replays" <| list profileReplayDecoder)
        (field "online" bool)


profileReplayDecoder : Decoder ProfileReplay
profileReplayDecoder =
    Json.map3 ProfileReplay
        (field "id" int)
        (field "pa" <| maybe string)
        (field "pb" <| maybe string)
