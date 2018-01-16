module Resolvable.State exposing (..)

import Animation.Types exposing (Anim)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Model.ViewModel


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
        (Maybe.map .model <| List.head model.resList)


activeAnim : Resolvable.Model -> Maybe Anim
activeAnim { resList } =
    case List.head resList of
        Just data ->
            data.anim

        Nothing ->
            Nothing


activeStackCard : Resolvable.Model -> Maybe Model.StackCard
activeStackCard { resList } =
    Maybe.map .stackCard <| List.head resList


nextActiveModel : Resolvable.Model -> Model.Model
nextActiveModel res =
    activeModel { res | resList = List.drop 1 res.resList }


tickStart : Resolvable.Model -> Bool
tickStart { tick } =
    tick == 0.0


tick : Float -> Resolvable.Model -> Resolvable.Model
tick dt model =
    if tickZero model.tick then
        resolveStep model
    else
        { model
            | tick = model.tick + dt
            , vm = Model.ViewModel.shakeDecay model.vm
        }


tickZero : Float -> Bool
tickZero tick =
    tick > resTickMax


resTickMax : Float
resTickMax =
    800.0


resolveStep : Resolvable.Model -> Resolvable.Model
resolveStep ({ vm, resList } as model) =
    case resList of
        r :: rs ->
            { vm = { vm | shake = 1.1 }
            , resList = rs
            , tick = 0
            , final = r.model
            }

        otherwise ->
            model


resolving : Resolvable.Model -> Bool
resolving { resList } =
    List.length resList > 0
