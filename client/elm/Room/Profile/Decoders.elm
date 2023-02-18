module Profile.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, bool, field, float, int, list, maybe, string)
import Profile.Types exposing (Profile, ProfileReplay)
import Stats exposing (levelFromExperience)


decoder : Decoder Profile
decoder =
    let
        makeProfile : String -> Float -> List ProfileReplay -> Bool -> Profile
        makeProfile name xp replays online =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            , replays = replays
            , online = online
            }
    in
    Json.map4 makeProfile
        (field "name" string)
        (field "xp" float)
        (field "replays" <| list profileReplayDecoder)
        (field "online" bool)


profileReplayDecoder : Decoder ProfileReplay
profileReplayDecoder =
    Json.map3 ProfileReplay
        (field "id" int)
        (field "pa" <| maybe string)
        (field "pb" <| maybe string)
