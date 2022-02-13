module Stats exposing (Experience, Level, StatChange, decodeStatChange, levelAt, levelFromExperience, levelToExperience, nextLevelAt, tick)

import Json.Decode as Json exposing (Decoder, field, float)


type alias Level =
    Int


type alias Experience =
    Float


type alias StatChange =
    { tick : Float
    , initialXp : Experience
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
            Json.map2 (StatChange 0)
                (field "initialExperience" float)
                (field "finalExperience" float)
    in
    Json.decodeString decoder msg


tick : Float -> Bool -> Maybe StatChange -> Maybe StatChange
tick dt resolving mStats =
    mStats
        |> Maybe.andThen
            (\stats ->
                let
                    newTick =
                        stats.tick + dt
                in
                if resolving then
                    Just stats

                else if newTick < 2500 then
                    Just { stats | tick = newTick }

                else
                    Nothing
            )
