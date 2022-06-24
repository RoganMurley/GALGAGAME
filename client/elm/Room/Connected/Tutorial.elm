module Tutorial exposing (..)

import Maybe.Extra as Maybe


type Action
    = ActionDragACard
    | ActionPressGo


type Stage
    = StageA Step
    | StageB


type alias Model =
    { step : Maybe Stage }


type Step
    = DragACard
    | PressGo
    | Finished


takeAction : Action -> Model -> Model
takeAction action model =
    case model.step of
        Just (StageA step) ->
            { model | step = Maybe.map StageA (takeActionStep action step) }

        _ ->
            model


takeActionStep : Action -> Step -> Maybe Step
takeActionStep action step =
    case ( action, step ) of
        ( ActionDragACard, DragACard ) ->
            Just PressGo

        ( ActionPressGo, PressGo ) ->
            Just Finished

        _ ->
            Just step


init : Model
init =
    { step = Nothing }


beginStageA : Model
beginStageA =
    { step = Just <| StageA DragACard }


beginStageB : Model
beginStageB =
    { step = Just StageB }


isActive : Model -> Bool
isActive { step } =
    Maybe.isJust step
