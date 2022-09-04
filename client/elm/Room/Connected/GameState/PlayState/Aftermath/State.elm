module Aftermath.State exposing (active, aftermathing, fromLeaderboard, fromStatChange, init, maxTick, saveUnlocks, skip, tick)

import Aftermath.Types as Aftermath exposing (Aftermath(..), Model)
import Leaderboard.Types exposing (Leaderboard)
import Main.Messages as Main
import PlayState.Messages as PlayState
import Ports exposing (saveUnlock)
import Stats exposing (StatChange)
import Util exposing (message)


init : Model
init =
    { tick = 0
    , aftermath = []
    , autoRematched = False
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

        Aftermath.Leaderboard _ ->
            999999999999999999

        Aftermath.Unlock _ ->
            999999999999999999


tick : Float -> Model -> ( Model, Cmd PlayState.Msg )
tick dt model =
    let
        newTick =
            model.tick + dt
    in
    case model.aftermath of
        a :: rest ->
            let
                newModel =
                    if newTick > maxTick a then
                        { model
                            | tick = 0
                            , aftermath = rest
                        }

                    else
                        { model
                            | tick = newTick
                            , aftermath = a :: rest
                        }
            in
            ( newModel, Cmd.none )

        _ ->
            let
                newModel =
                    { tick = newTick
                    , aftermath = []
                    , autoRematched = True
                    }

                msg =
                    if model.autoRematched then
                        Cmd.none

                    else
                        message <|
                            PlayState.PlayingOnly
                                PlayState.Continue
            in
            ( newModel, msg )


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
    , autoRematched = False
    }


fromLeaderboard : Model -> Leaderboard -> Model
fromLeaderboard model leaderboard =
    { tick = 0
    , aftermath = model.aftermath ++ [ Aftermath.Leaderboard leaderboard ]
    , autoRematched = False
    }


skip : Model -> Model
skip model =
    { model
        | aftermath = List.drop 1 model.aftermath
        , tick = 0
    }


saveUnlocks : StatChange -> Cmd Main.Msg
saveUnlocks { unlocks } =
    case unlocks of
        unlock :: _ ->
            saveUnlock unlock.name

        _ ->
            Cmd.none
