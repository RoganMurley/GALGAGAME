module Profile.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, int, list, maybe, string)
import Profile.Types exposing (Profile, ProfileReplay)
import Stats exposing (levelFromExperience)


decoder : Decoder Profile
decoder =
    let
        makeProfile : String -> Float -> List ProfileReplay -> Profile
        makeProfile name xp replays =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            , replays = replays
            }
    in
    Json.map3 makeProfile
        (field "name" string)
        (field "xp" float)
        (field "replays" <| list profileReplayDecoder)


profileReplayDecoder : Decoder ProfileReplay
profileReplayDecoder =
    Json.map3 ProfileReplay
        (field "id" int)
        (field "pa" <| maybe string)
        (field "pb" <| maybe string)
