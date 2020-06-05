module RuneSelect.State exposing (update)

import Carousel
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextRune ->
            { model | carousel = Carousel.forward model.carousel }

        PreviousRune ->
            { model | carousel = Carousel.backward model.carousel }
