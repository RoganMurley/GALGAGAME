module Stats exposing (Experience, Level, StatChange, decodeStatChange)

import Json.Decode as Json exposing (Decoder, field, int)


type alias Level =
    Int


type alias Experience =
    Int


type alias StatChange =
    { initialLevel : Level
    , initialExperience : Experience
    , finalLevel : Level
    , finalExperience : Experience
    , nextLevelAt : Experience
    }


decodeStatChange : String -> Result Json.Error StatChange
decodeStatChange msg =
    let
        decoder : Decoder StatChange
        decoder =
            Json.map5 StatChange
                (field "initialLevel" int)
                (field "initialExperience" int)
                (field "finalLevel" int)
                (field "finalExperience" int)
                (field "nextLevelAt" int)
    in
    Json.decodeString decoder msg
