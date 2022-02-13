module Stats exposing (Experience, Level, StatChange, decodeStatChange, levelAt, levelFromExperience, levelToExperience, nextLevelAt)

import Json.Decode as Json exposing (Decoder, field, float)


type alias Level =
    Int


type alias Experience =
    Float


type alias StatChange =
    { initialXp : Experience
    , finalXp : Experience
    }


levellingConstant : Float
levellingConstant =
    0.1


levelFromExperience : Experience -> Level
levelFromExperience xp =
    1 + (floor <| levellingConstant * sqrt xp)


levelToExperience : Level -> Experience
levelToExperience level =
    ((toFloat level - 1) / levellingConstant) ^ 2


nextLevelAt : Experience -> Experience
nextLevelAt =
    levelFromExperience >> (+) 1 >> levelToExperience


levelAt : Experience -> Experience
levelAt =
    levelFromExperience >> levelToExperience


decodeStatChange : String -> Result Json.Error StatChange
decodeStatChange msg =
    let
        decoder : Decoder StatChange
        decoder =
            Json.map2 StatChange
                (field "initialExperience" float)
                (field "finalExperience" float)
    in
    Json.decodeString decoder msg
