module RuneSelect.State exposing (tick, update)

import Carousel
import Game.Types exposing (Context)
import RuneSelect.Entities as RuneSelect
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextRune ->
            { model | carousel = Carousel.forward model.carousel }

        PreviousRune ->
            { model | carousel = Carousel.backward model.carousel }


tick : Context -> Float -> Model -> Model
tick ctx _ model =
    { model
        | entities = RuneSelect.entities ctx model
    }
