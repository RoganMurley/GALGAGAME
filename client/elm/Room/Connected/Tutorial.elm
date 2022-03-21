module Tutorial exposing (..)

import Maybe.Extra as Maybe


type Action
    = ActionDragACard
    | ActionPressGo


type alias Model =
    { step : Maybe Step }


type Step
    = DragACard
    | PressGo
    | Finished


takeAction : Action -> Model -> Model
takeAction action model =
    case Maybe.map (takeActionStep action) model.step of
        Just newStep ->
            { model | step = newStep }

        Nothing ->
            model


takeActionStep : Action -> Step -> Maybe Step
takeActionStep action step =
    case ( action, step ) of
        ( ActionDragACard, DragACard ) ->
            Just PressGo

        ( ActionPressGo, PressGo ) ->
            Nothing

        _ ->
            Just step


init : Model
init =
    { step = Nothing }


begin : Model
begin =
    { step = Just DragACard }


isActive : Model -> Bool
isActive { step } =
    Maybe.isJust step
