module Aftermath.State exposing (active, aftermathing, fromStatChange, init, maxTick, skip, tick)

import Aftermath.Types as Aftermath exposing (Aftermath(..), Model)
import Stats exposing (StatChange)


init : Model
init =
    { tick = 0
    , aftermath = []
    }


aftermathing : Model -> Bool
aftermathing { aftermath } =
    List.length aftermath > 0


active : Model -> Maybe Aftermath
active { aftermath } =
    List.head aftermath


maxTick : Aftermath -> Float
maxTick aftermath =
    case aftermath of
        Aftermath.Winner ->
            1000

        Aftermath.StatChange _ t ->
            t

        Aftermath.Unlock _ ->
            999999999999999999


tick : Float -> Model -> Model
tick dt model =
    let
        newTick =
            model.tick + dt
    in
    case model.aftermath of
        a :: rest ->
            if newTick > maxTick a then
                { tick = 0
                , aftermath = rest
                }

            else
                { tick = newTick
                , aftermath = a :: rest
                }

        _ ->
            { tick = newTick
            , aftermath = []
            }


fromStatChange : StatChange -> Model
fromStatChange stats =
    let
        initialStats =
            { initialXp = stats.initialXp
            , finalXp = stats.initialXp
            }

        midStats =
            { initialXp = stats.initialXp
            , finalXp = stats.finalXp
            }

        finalStats =
            { initialXp = stats.finalXp
            , finalXp = stats.finalXp
            }
    in
    { tick = 0
    , aftermath =
        [ Winner
        , Aftermath.StatChange initialStats 500
        , Aftermath.StatChange midStats 1000
        , Aftermath.StatChange finalStats 1500
        ]
            ++ List.map Aftermath.Unlock stats.unlocks
    }


skip : Model -> Model
skip model =
    { model
        | aftermath = List.drop 1 model.aftermath
        , tick = 0
    }