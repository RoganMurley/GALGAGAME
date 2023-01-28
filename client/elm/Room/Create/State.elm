module Create.State exposing (init, update, validator)

import Create.Messages exposing (Msg(..))
import Create.Types exposing (Field(..), Model)
import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Json.Encode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Util exposing (message)


init : Model
init =
    { allowSpectators = initFormField
    , startingLife = initFormField
    , error = ""
    , submitting = False
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input AllowSpectators allowSpectators ->
            ( { model | startingLife = updateFormField allowSpectators model.allowSpectators }
            , Cmd.none
            )

        Input StartingLife startingLife ->
            ( { model | startingLife = updateFormField startingLife model.startingLife }
            , Cmd.none
            )

        Submit ->
            let
                name =
                    "foo"

                encoded =
                    Json.encode 0
                        (Json.object
                            [ ( "allowSpectators", Json.bool <| Form.toBool model.allowSpectators )
                            , ( "startingLife", Json.string model.startingLife.value )
                            , ( "name", Json.string name )
                            ]
                        )
            in
            ( { model | submitting = True }
            , Cmd.batch
                [ message <| Main.Send <| "play"
                , message <| Main.Send <| "createCustomRoom:" ++ encoded
                ]
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
