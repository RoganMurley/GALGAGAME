module RuneSelect.State exposing (tick, update)

import Carousel
import Collision exposing (hitTest)
import Game.Types exposing (Context)
import List.Extra as List
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
    let
        newEntities =
            RuneSelect.entities ctx model

        newHover =
            Maybe.andThen
                (\pos -> List.find (hitTest pos 32) newEntities)
                ctx.mouse
    in
    { model
        | entities = newEntities
        , hover = newHover
    }
