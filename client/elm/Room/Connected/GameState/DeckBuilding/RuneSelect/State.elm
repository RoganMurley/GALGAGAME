module RuneSelect.State exposing (buttons, tick, update)

import Buttons.State as Buttons
import Buttons.Types exposing (Buttons)
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
    let
        newEntities =
            RuneSelect.entities ctx model

        newHover =
            Nothing

        -- Maybe.andThen
        --     (\pos -> List.find (hitTest pos 32) newEntities)
        --     ctx.mouse
    in
    { model
        | entities = newEntities
        , hover = newHover
    }


buttons : Context -> Float -> Model -> Buttons
buttons ctx dt model =
    Buttons.empty
