module Resolvable.State exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(NullAnim))
import Model.Diff as Model
import Model.Types as Model
import Resolvable.Types as Resolvable
import Model.ViewModel
import Stack.Types exposing (StackCard)
import Util exposing (zip)


init : Model.Model -> List Resolvable.ResolveData -> Resolvable.Model
init model resList =
    { vm = Model.ViewModel.init
    , tick = 0
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


activeStackCard : Resolvable.Model -> Maybe StackCard
activeStackCard { resList } =
    List.head resList |> Maybe.andThen .stackCard


tickStart : Resolvable.Model -> Bool
tickStart { tick } =
    tick == 0.0


tick : Float -> Resolvable.Model -> Resolvable.Model
tick dt model =
    if tickEnd model.tick (activeAnim model) then
        resolveStep model
    else
        { model
            | tick = model.tick + dt
        }


tickEnd : Float -> Anim -> Bool
tickEnd tick anim =
    tick >= Animation.animMaxTick anim


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
        combine ( m, { anim, stackCard } ) =
            { model = m
            , anim = anim
            , stackCard = stackCard
            }
    in
        List.map combine <|
            zip models resDiffs
