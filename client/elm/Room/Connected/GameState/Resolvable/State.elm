module Resolvable.State exposing (..)

import Animation.State exposing (animToShake, animToResTickMax)
import Animation.Types exposing (Anim)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Model.ViewModel
import Stack.Types exposing (StackCard)


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
    List.head resList |> Maybe.andThen .anim


activeStackCard : Resolvable.Model -> Maybe StackCard
activeStackCard { resList } =
    List.head resList |> Maybe.andThen .stackCard


tickStart : Resolvable.Model -> Bool
tickStart { tick } =
    tick == 0.0


tick : Float -> Resolvable.Model -> Resolvable.Model
tick dt model =
    if tickZero model.tick (activeAnim model) then
        resolveStep model
    else
        { model
            | tick = model.tick + dt
            , vm = Model.ViewModel.shakeDecay model.vm
        }


tickZero : Float -> Maybe Anim -> Bool
tickZero tick anim =
    tick > animToResTickMax anim


resolveStep : Resolvable.Model -> Resolvable.Model
resolveStep ({ vm, resList, final } as model) =
    case resList of
        r :: rs ->
            shakeStep
                { vm = vm
                , resList = rs
                , tick = 0
                , final = final
                }

        otherwise ->
            model


shakeStep : Resolvable.Model -> Resolvable.Model
shakeStep ({ vm } as model) =
    let
        shake =
            Maybe.withDefault 0 <|
                Maybe.map animToShake <|
                    activeAnim model

        newVm =
            { vm | shake = shake }
    in
        { model | vm = newVm }


resolving : Resolvable.Model -> Bool
resolving { resList } =
    List.length resList > 0
