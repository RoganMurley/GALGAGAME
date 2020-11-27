module RuneSelect.State exposing (buttons, tick, update)

import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Carousel
import Collision
import Game.Types exposing (Context)
import List.Extra as List
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
            ctx.mouseRay
                |> Maybe.andThen
                    (\ray ->
                        List.find
                            (Collision.hitTest3d ray 0.18)
                            newEntities
                    )

        newButtons =
            buttons ctx dt model
    in
    { model
        | entities = newEntities
        , hover = newHover
        , buttons = newButtons
    }


buttons : Context -> Float -> Model -> Buttons
buttons { mouse, radius, w, h } dt model =
    let
        arrowScale =
            radius * 0.1

        arrowOffset =
            radius * 0.43
    in
    Buttons.fromList <|
        List.map (\f -> f dt mouse model.buttons)
            [ Buttons.entity
                "nextRune"
                { x = 0.5 * w + arrowOffset
                , y = 0.9 * h
                , width = arrowScale
                , height = arrowScale
                , btn =
                    ImageButton
                        { img = "next.png"
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        }
                , disabled = False
                }
            , Buttons.entity
                "prevRune"
                { x = 0.5 * w - arrowOffset
                , y = 0.9 * h
                , width = -arrowScale
                , height = arrowScale
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
                , width = 0.25 * radius
                , height = 0.1 * radius
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
