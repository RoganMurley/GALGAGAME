module Profile.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, string)
import Profile.Types exposing (Profile)
import Stats exposing (levelFromExperience)


decoder : Decoder Profile
decoder =
    let
        makeProfile : String -> Float -> Profile
        makeProfile name xp =
            { name = name
            , xp = xp
            , level = levelFromExperience xp
            }
    in
    Json.map2 makeProfile
        (field "name" string)
        (field "xp" float)
