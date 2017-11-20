module Connected.State exposing (init, update)

import Connected.Types exposing (..)


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        otherwise ->
            ( model, Cmd.none )
