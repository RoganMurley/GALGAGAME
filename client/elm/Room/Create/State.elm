module Create.State exposing (init, update)

import Create.Messages exposing (Msg(..))
import Create.Types exposing (Model)
import Html.Attributes exposing (start)
import Main.Messages as Main
import Main.Types exposing (Flags)


init : Model
init =
    { allowSpectators = True
    , startingLife = 50
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        SetAllowSpectators allowSpectators ->
            ( { model | allowSpectators = allowSpectators }
            , Cmd.none
            )

        SetStartingLife startingLife ->
            ( { model | startingLife = startingLife }
            , Cmd.none
            )
