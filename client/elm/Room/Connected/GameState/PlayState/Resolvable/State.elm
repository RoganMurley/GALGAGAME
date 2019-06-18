module Resolvable.State exposing (activeAnim, activeAnimDamage, activeModel, activeStackCard, init, merge, resDiffToData, resolveStep, resolving, tick, tickEnd, tickStart)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import List.Extra as List
import Model.Diff as Model
import Model.Types as Model
import Resolvable.Types as Resolvable
import Stack.Types exposing (StackCard)
import Util exposing (zip)


init : Model.Model -> List Resolvable.ResolveData -> Resolvable.Model
init model resList =
    { tick = 0
    , final = model
    , resList = resList
    }


activeModel : Resolvable.Model -> Model.Model
activeModel model =
    Maybe.withDefault
        model.final
    <|
        Maybe.map .model <|
            List.head model.resList


activeAnim : Resolvable.Model -> Anim
activeAnim { resList } =
    case List.head resList of
        Just res ->
            res.anim

        Nothing ->
            NullAnim


activeAnimDamage : Resolvable.Model -> ( Float, Float )
activeAnimDamage { resList } =
    case List.head resList of
        Just res ->
            res.animDamage

        Nothing ->
            ( 0, 0 )


activeStackCard : Resolvable.Model -> Maybe StackCard
activeStackCard { resList } =
    List.head resList |> Maybe.andThen .stackCard


tickStart : Resolvable.Model -> Bool
tickStart model =
    model.tick == 0.0


tick : Float -> Resolvable.Model -> Resolvable.Model
tick dt model =
    if tickEnd model.tick (activeAnim model) then
        resolveStep model

    else
        { model
            | tick = model.tick + dt
        }


tickEnd : Float -> Anim -> Bool
tickEnd tickValue anim =
    tickValue >= Animation.animMaxTick anim


resolveStep : Resolvable.Model -> Resolvable.Model
resolveStep model =
    case model.resList of
        _ :: rs ->
            { model
                | resList = rs
                , tick = 0
            }

        _ ->
            { model | tick = 0 }


resolving : Resolvable.Model -> Bool
resolving { resList } =
    List.length resList > 0


merge : Model.Model -> Resolvable.ResolveDiffData -> Resolvable.ResolveData
merge model diffData =
    { model = Model.merge diffData.diff model
    , anim = diffData.anim
    , animDamage = diffData.animDamage
    , stackCard = diffData.stackCard
    }


resDiffToData : Model.Model -> List Resolvable.ResolveDiffData -> List Resolvable.ResolveData
resDiffToData model resDiffs =
    let
        diffs : List Model.Diff
        diffs =
            List.map .diff resDiffs

        models : List Model.Model
        models =
            List.drop 1 <|
                List.scanl Model.merge model diffs

        combine : ( Model.Model, Resolvable.ResolveDiffData ) -> Resolvable.ResolveData
        combine ( m, { anim, animDamage, stackCard } ) =
            { model = m
            , anim = anim
            , animDamage = animDamage
            , stackCard = stackCard
            }
    in
    List.map combine <|
        zip models resDiffs
