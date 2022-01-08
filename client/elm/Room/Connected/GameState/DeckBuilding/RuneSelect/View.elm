module RuneSelect.View exposing (view)

import Card.View as Card
import Colour
import Font.View as Font
import Game.Types exposing (Context, Focus(..))
import Math.Matrix4 exposing (makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model.View exposing (focusImageView, focusTextView)
import Quaternion
import Render.Primitives
import Render.Shaders
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model, Rune, RuneCursor(..))
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Model -> Context -> List WebGL.Entity
view model ({ w, h, tick } as ctx) =
    let
        size =
            1.4 * max w h

        focus =
            Maybe.map
                (\hover -> { owner = PlayerA, card = hover.card })
                model.hover
    in
    List.concat
        [ List.concat <| List.map (Card.view ctx) model.entities
        , focusView model.carousel.selected focus ctx
        , Font.view
            "Futura"
            model.carousel.selected.name
            { x = w * 0.5
            , y = h * 0.1
            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]


focusView : Rune -> Maybe StackCard -> Context -> List WebGL.Entity
focusView rune focus ({ w, h, textures } as ctx) =
    case focus of
        Just focusCard ->
            List.concat
                [ focusImageView
                    (vec3 0 0.3 0)
                    (FocusCard focusCard)
                    ctx
                , focusTextView (vec2 0 (-h * 0.1)) (FocusCard focusCard) ctx
                ]

        _ ->
            let
                { a, b, c, d } =
                    rune.cards

                eachView imgURL { rot, x, y, scale } =
                    Texture.with textures imgURL <|
                        \texture ->
                            [ Render.Primitives.quad Render.Shaders.fragment <|
                                { rotation = Quaternion.makeRotate <| Quaternion.zRotation rot
                                , scale = makeScale3 scale scale 1
                                , color = Colour.white
                                , pos = vec3 x y (4 + 3 * (h / w))
                                , perspective = ctx.perspective
                                , camera = ctx.camera3d
                                , texture = texture
                                }
                            ]
            in
            List.concat
                [ -- SWORD
                  eachView a.imgURL
                    { rot = -0.25 * pi
                    , scale = 1
                    , x = 1.5
                    , y = 0.7
                    }

                -- WAND
                , eachView b.imgURL
                    { rot = 0
                    , scale = 1
                    , x = -1.5
                    , y = 0.7
                    }

                -- CUP
                , eachView c.imgURL
                    { rot = 0
                    , scale = 0.7
                    , x = 0
                    , y = 0
                    }

                -- COIN
                , eachView d.imgURL
                    { rot = 0
                    , scale = 0.8
                    , x = 0
                    , y = 1.4
                    }
                ]
