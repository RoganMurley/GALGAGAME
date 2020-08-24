module RuneSelect.State exposing (buttons, tick, update)

import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import Game.Types exposing (Context)
import Math.Vector3 exposing (vec3)
import RuneSelect.Entities as RuneSelect
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextRune ->
            let
                newButtons =
                    Buttons.update
                        "nextRune"
                        (\b -> { b | hover = 0 })
                        model.buttons
            in
            { model | carousel = Carousel.forward model.carousel, buttons = newButtons }

        PreviousRune ->
            let
                newButtons =
                    Buttons.update
                        "prevRune"
                        (\b -> { b | hover = 0 })
                        model.buttons
            in
            { model | carousel = Carousel.backward model.carousel, buttons = newButtons }


tick : Context -> Float -> Model -> Model
tick ctx dt model =
    let
        newEntities =
            RuneSelect.entities ctx model

        newHover =
            Nothing

        -- Maybe.andThen
        --     (\pos -> List.find (hitTest pos 32) newEntities)
        --     ctx.mouse
        newButtons =
            buttons ctx dt model
    in
    { model
        | entities = newEntities
        , hover = newHover
        , buttons = newButtons
    }


buttons : Context -> Float -> Model -> Buttons
buttons { mouse, w, h } dt model =
    Buttons.fromList <|
        List.map (\f -> f dt mouse model.buttons)
            [ Buttons.entity
                "nextRune"
                { x = 0.7 * w
                , y = 0.5 * h
                , xScale = 0.6 * max w h
                , yScale = 0.6 * max w h
                , btn =
                    ImageButton
                        { img = "next.png"
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        }
                , disabled = False
                }
            , Buttons.entity
                "prevRune"
                { x = 0.3 * w
                , y = 0.5 * h
                , xScale = -0.6 * max w h
                , yScale = 0.6 * max w h
                , btn =
                    ImageButton
                        { img = "next.png"
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        }
                , disabled = False
                }
            , Buttons.entity
                "selectRune"
                { x = 0.5 * w
                , y = 0.9 * h
                , xScale = 1.6 * max w h
                , yScale = 0.6 * max w h
                , btn =
                    TextButton
                        { font = "Futura"
                        , text = "Choose?"
                        , textColor = vec3 (0 / 255) (0 / 255) (80 / 255)
                        , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , options = [ Buttons.HoverText "Choose!" ]
                        }
                , disabled = False
                }
            ]
