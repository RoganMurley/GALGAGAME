module Create.State exposing (init, update, validator)

import Create.Messages exposing (Msg(..))
import Create.Types exposing (Field(..), Model)
import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Main.Messages as Main
import Main.Types exposing (Flags)


init : Model
init =
    { allowSpectators = True
    , startingLife = initFormField
    , error = ""
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        SetAllowSpectators allowSpectators ->
            ( { model | allowSpectators = allowSpectators }
            , Cmd.none
            )

        Input StartingLife startingLife ->
            ( { model | startingLife = updateFormField startingLife model.startingLife }
            , Cmd.none
            )


startingLifeValidator : Validator Model Field
startingLifeValidator { startingLife } =
    case String.toInt startingLife.value of
        Just int ->
            if int < 0 then
                [ { field = StartingLife
                  , error = Error "Must be positive"
                  , touched = startingLife.touched
                  }
                ]

            else if int > 999 then
                [ { field = StartingLife
                  , error = Error "Must be lower than 1000"
                  , touched = startingLife.touched
                  }
                ]

            else
                []

        Nothing ->
            [ { field = StartingLife
              , error = Error "Must be an integer"
              , touched = startingLife.touched
              }
            ]


validator : Model -> List (ValidationResult Field)
validator =
    batchValidators [ startingLifeValidator ]
