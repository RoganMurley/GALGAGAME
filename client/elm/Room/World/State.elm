module World.State exposing (init, receive, tick, update)

import Main.Messages as Main
import Main.Types exposing (Flags)
import World.Messages exposing (Msg(..))
import World.Types exposing (Model)


init : Model
init =
    { time = 0 }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg _ =
    case msg of
        Msg ->
            ( model, Cmd.none )


tick : Model -> Float -> Model
tick model dt =
    { time = model.time + dt }


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none
