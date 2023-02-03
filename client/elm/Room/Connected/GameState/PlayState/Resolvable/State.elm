module Resolvable.State exposing (activeAnim, activeAnimDamage, activeModel, goto, init, isPremovable, merge, resDiffToData, resolveStep, resolving, tick, tickEnd, tickStart)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import List.Extra as List
import Model.Diff as Model
import Model.Types as Model
import Resolvable.Types as Resolvable
import Util exposing (zip)
import WhichPlayer.Types exposing (WhichPlayer(..))


init : Model.Model -> List Resolvable.ResolveData -> Resolvable.Model
init model resList =
    { tick = 0
    , final = model
    , resList = resList
    , history = []
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


tickStart : Resolvable.Model -> Bool
tickStart model =
    model.tick == 0.0


tick : Float -> Resolvable.Model -> Resolvable.Model
tick dt model =
    if tickEnd model.tick (activeAnim model) then
        resolveStep model

    else if model.tick + dt < 0 then
        resolveReverseStep model dt

    else
        { model
            | tick =
                model.tick + dt
        }


tickEnd : Float -> Anim -> Bool
tickEnd tickValue anim =
    tickValue >= Animation.animMaxTick anim


resolveStep : Resolvable.Model -> Resolvable.Model
resolveStep model =
    case model.resList of
        r :: rs ->
            { model
                | resList = rs
                , tick = 0
                , history = r :: model.history
            }

        _ ->
            { model | tick = 0 }


resolveReverseStep : Resolvable.Model -> Float -> Resolvable.Model
resolveReverseStep model dt =
    case model.history of
        h :: hs ->
            let
                newModel =
                    { model
                        | resList = h :: model.resList
                        , history = hs
                    }
            in
            { newModel
                | tick = Animation.animMaxTick (activeAnim newModel) + model.tick + dt
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
        combine ( m, { anim, animDamage } ) =
            { model = m
            , anim = anim
            , animDamage = animDamage
            }
    in
    List.map combine <|
        zip models resDiffs


isPremovable : Resolvable.Model -> Bool
isPremovable { resList } =
    let
        f : Resolvable.ResolveData -> Bool
        f { anim } =
            case anim of
                NullAnim ->
                    True

                Pass _ ->
                    True

                Play _ _ _ _ ->
                    True

                Rotate _ ->
                    True

                Windup _ ->
                    True

                _ ->
                    False
    in
    List.all f resList


goto : Float -> Resolvable.Model -> Resolvable.Model
goto frame res =
    let
        maxTick =
            Animation.animMaxTick <| activeAnim res

        newFrame =
            frame - maxTick
    in
    if newFrame >= 0 then
        goto newFrame <| resolveStep res

    else
        { res | tick = frame }
