module Chat.State exposing (init, update)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Main.Messages as Main
import Mouse exposing (Position)
import Util


init : Model
init =
    { input = ""
    , messages = []
    , pos = Position 0 0
    , drag = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        Input str ->
            ( { model | input = str }, Cmd.none )

        New str ->
            ( appendMessage str model, Cmd.none )

        Send ->
            let
                str : String
                str =
                    model.input

                cmd : Cmd Main.Msg
                cmd =
                    case str of
                        "" ->
                            Cmd.none

                        otherwise ->
                            Util.message <| Main.Send <| "chat:" ++ str
            in
                ( clearInput <| model, cmd )


clearInput : Model -> Model
clearInput model =
    { model | input = "" }


appendMessage : String -> Model -> Model
appendMessage str model =
    { model | messages = str :: model.messages }
