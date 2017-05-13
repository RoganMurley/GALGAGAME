module Chat.State exposing (addMessage, clearInput, init)

import Mouse exposing (Position)
import Chat.Types exposing (Model)


init : Model
init =
    { input = ""
    , messages = []
    , pos = Position 0 0
    , drag = Nothing
    }


clearInput : Model -> Model
clearInput model =
    { model | input = "" }


addMessage : String -> Model -> Model
addMessage message model =
    { model | messages = message :: model.messages }
