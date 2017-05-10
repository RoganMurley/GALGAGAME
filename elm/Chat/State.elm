module Chat.State exposing (addMessage, init)

import Mouse exposing (Position)
import Chat.Types exposing (Model)


init : Model
init =
    { input = ""
    , messages = []
    , pos = Position 0 0
    , drag = Nothing
    }


addMessage : String -> Model -> Model
addMessage message model =
    { model | messages = message :: model.messages }
