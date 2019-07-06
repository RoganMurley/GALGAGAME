module Feedback.View exposing (view)

import Feedback.Messages exposing (Msg(..))
import Feedback.Types exposing (Field(..), Model, SubmitState(..))
import Form exposing (Error(..), FormFieldClass, FormFieldType(..), formInputView)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (autofocus, class, disabled)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    let
        formFieldClass : FormFieldClass Field Msg
        formFieldClass =
            { getFieldPlaceholder = always "Humans will read this..."
            , getFieldLabel = always Nothing
            , getFormFieldType = always TextAreaType
            , getExtraAttrs = always [ autofocus True ]
            , getInputMsg = Input
            , getIdentifier = always "body"
            }
    in
    div []
        [ div [ class "feedback-box" ]
            (case model.submitState of
                Submitted ->
                    [ h1 [] [ text "Thank you for your feedback!" ]

                    --, button [ onClick Continue ] [ text "Continue" ]
                    ]

                _ ->
                    [ formInputView formFieldClass [] Body
                    , button
                        [ onClick Submit
                        , disabled <| model.submitState == Submitting
                        ]
                        [ text "Submit feedback" ]
                    , div [ class "error" ] [ text model.error ]
                    ]
            )
        ]
