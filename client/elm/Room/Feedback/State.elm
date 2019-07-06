module Feedback.State exposing (init, receive, update)

import Browser.Navigation
import Feedback.Decoders exposing (feedbackErrorDecoder)
import Feedback.Messages exposing (Msg(..))
import Feedback.Types exposing (Field(..), Model, SubmitState(..))
import Form exposing (Error(..), initFormField, updateFormField)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (Key(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Room.Messages as Room
import Util exposing (authLocation)


init : Maybe String -> Model
init nextUrl =
    { body = initFormField
    , error = ""
    , submitState = NotSubmitted
    , nextUrl = Maybe.withDefault "/" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Body body ->
            ( { model | body = updateFormField body model.body }, Cmd.none )

        Submit ->
            ( { model | submitState = Submitting, error = "" }
            , Http.send
                (Main.RoomMsg << Room.FeedbackMsg << SubmitCallback)
              <|
                Http.post
                    (authLocation flags ++ "/feedback")
                    (Http.multipartBody
                        [ Http.stringPart "body" model.body.value
                        ]
                    )
                    (maybe feedbackErrorDecoder)
            )

        SubmitCallback (Ok (Just { error })) ->
            ( { model | error = error, submitState = NotSubmitted }, Cmd.none )

        SubmitCallback (Ok Nothing) ->
            ( { model | submitState = Submitted }, Cmd.none )

        SubmitCallback (Err _) ->
            ( { model
                | error = "Error connecting to server"
                , submitState = NotSubmitted
              }
            , Cmd.none
            )

        Continue ->
            ( model, Browser.Navigation.pushUrl flags.key model.nextUrl )


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none
