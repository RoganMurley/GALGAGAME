module Tutorial exposing (..)


type Action
    = ActionDragACard
    | ActionPressGo


type Stage
    = StageBasic Int
    | StageA Int
    | StageB


type alias Model =
    { step : Maybe Stage }


takeAction : Action -> Model -> Model
takeAction action model =
    case model.step of
        Just (StageBasic step) ->
            let
                newStep : Int
                newStep =
                    case action of
                        ActionPressGo ->
                            if step == 0 then
                                step + 1

                            else
                                step

                        ActionDragACard ->
                            if step == 1 then
                                step + 1

                            else
                                step
            in
            { model | step = Just (StageBasic newStep) }

        Just (StageA step) ->
            let
                newStep : Int
                newStep =
                    case ( action, step ) of
                        ( ActionPressGo, 0 ) ->
                            step + 1

                        ( ActionDragACard, 1 ) ->
                            step + 1

                        _ ->
                            step
            in
            { model | step = Just (StageA newStep) }

        _ ->
            model


init : Model
init =
    { step = Nothing }


beginStageBasic : Model
beginStageBasic =
    { step = Just (StageBasic 0) }


beginStageA : Model
beginStageA =
    { step = Just (StageA 0) }


beginStageB : Model
beginStageB =
    { step = Just StageB }


isActive : Model -> Bool
isActive { step } =
    case step of
        Just (StageA _) ->
            False

        Just (StageBasic i) ->
            i < 2

        _ ->
            False
