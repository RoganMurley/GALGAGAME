module Endgame.View exposing (animView, buttonEntities, view)

import Aftermath.State as Aftermath
import Aftermath.Types as Aftermath exposing (Aftermath(..))
import Animation.Types exposing (Anim(..))
import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Buttons.View as Buttons
import Colour
import Ease
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import GameType exposing (GameType(..))
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Quaternion
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import RuneSelect.Types exposing (Rune)
import Stats as Stats
import Texture.State as Texture
import Util exposing (interpFloat)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


backgroundView : Context -> List WebGL.Entity
backgroundView ctx =
    let
        { w, h, progress, anim, camera2d, ortho } =
            ctx
    in
    case anim of
        GameEnd winner ->
            let
                backgroundColor =
                    case winner of
                        Just PlayerA ->
                            vec3 (30 / 255) (200 / 255) (30 / 255)

                        Just PlayerB ->
                            vec3 (200 / 255) (30 / 255) (30 / 255)

                        Nothing ->
                            vec3 (255 / 255) (255 / 255) (255 / 255)
            in
            [ Render.Primitives.quad Render.Shaders.matte
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 w h 1
                , color = backgroundColor
                , alpha = 0.8 * Ease.outCubic progress
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                }
            ]

        _ ->
            []


animView : Context -> List WebGL.Entity
animView ctx =
    let
        { w, h, radius, anim, progress } =
            ctx
    in
    case anim of
        GameEnd winner ->
            let
                text =
                    case winner of
                        Just PlayerA ->
                            "VICTORY"

                        Just PlayerB ->
                            "DEFEAT"

                        Nothing ->
                            "DRAW"

                color =
                    vec3 (244 / 255) (241 / 255) (94 / 255)

                shadowOffsetX =
                    0.009 * radius

                scale =
                    0.0008 * radius
            in
            List.concat
                [ backgroundView ctx
                , Font.view
                    "Futura"
                    text
                    { x = w * 0.5 - shadowOffsetX
                    , y = h * 0.5
                    , scaleX = scale * Ease.outBounce (progress * progress)
                    , scaleY = scale * Ease.outBounce (progress * progress)
                    , color = vec3 (40 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    text
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = scale * Ease.outBounce progress
                    , scaleY = scale * Ease.outBounce progress
                    , color = color
                    }
                    ctx
                ]

        _ ->
            []


view : Render.Params -> Assets.Model -> Maybe WhichPlayer -> Bool -> Aftermath.Model -> Buttons -> List WebGL.Entity
view { w, h } assets winner resolving aftermath buttons =
    let
        ctx =
            bareContextInit ( w, h ) assets NoMouse
    in
    if resolving then
        []

    else
        List.concat <|
            case Aftermath.active aftermath of
                Just Aftermath.Winner ->
                    [ animView { ctx | anim = GameEnd winner, progress = 1 } ]

                Just (Aftermath.StatChange stats t) ->
                    [ backgroundView { ctx | anim = GameEnd winner, progress = 1 }
                    , xpView stats aftermath.tick t ctx
                    ]

                Just (Aftermath.Unlock rune) ->
                    [ backgroundView { ctx | anim = GameEnd winner, progress = 1 }
                    , unlockView ctx aftermath.tick rune
                    ]

                Nothing ->
                    [ backgroundView { ctx | anim = GameEnd winner, progress = 1 }
                    , Buttons.view buttons ctx
                    ]


xpView : { initialXp : Float, finalXp : Float } -> Float -> Float -> Context -> List WebGL.Entity
xpView stats tick maxTick ctx =
    let
        { initialXp, finalXp } =
            stats

        { camera2d, ortho, w, h, radius } =
            ctx

        start =
            currentXp - Stats.levelAt currentXp

        end =
            Stats.nextLevelAt currentXp - Stats.levelAt currentXp

        progress =
            Ease.outQuad <|
                clamp 0 1 <|
                    (tick / maxTick)

        currentXp =
            interpFloat progress initialXp finalXp

        color =
            vec3 (244 / 255) (241 / 255) (94 / 255)

        colorDark =
            vec3 (40 / 255) (20 / 255) (20 / 255)

        donutEntities =
            [ Render.Primitives.donut
                { rotation = makeRotate 0 (vec3 0 0 1)
                , scale = makeScale3 -(0.62 * radius) (0.62 * radius) 1
                , color = color
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , mag = 0
                , thickness = 0.415
                }
            , Render.Primitives.donut
                { rotation = makeRotate 0 (vec3 0 0 1)
                , scale = makeScale3 -(0.6 * radius) (0.6 * radius) 1
                , color = vec3 (0 / 255) (0 / 255) (81 / 255)
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , mag = 0
                , thickness = 0.4
                }
            , Render.Primitives.donut
                { rotation = makeRotate 0 (vec3 0 0 1)
                , scale = makeScale3 -(0.6 * radius) (0.6 * radius) 1
                , color = color
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , mag = 1 - (start / end)
                , thickness = 0.4
                }
            ]

        textEntities =
            Font.view
                "Futura"
                ("LEVEL " ++ String.fromInt (Stats.levelFromExperience currentXp))
                { x = (w * 0.5) - (0.008 * radius)
                , y = h * 0.5
                , scaleX = 0.00035 * radius
                , scaleY = 0.00035 * radius
                , color = colorDark
                }
                ctx
                ++ Font.view
                    "Futura"
                    ("LEVEL " ++ String.fromInt (Stats.levelFromExperience currentXp))
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.00035 * radius
                    , scaleY = 0.00035 * radius
                    , color = color
                    }
                    ctx
    in
    donutEntities ++ textEntities


unlockView : Context -> Float -> Rune -> List WebGL.Entity
unlockView ctx _ rune =
    let
        { textures, w, h, radius } =
            ctx

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
            ++ Font.view
                "Futura"
                (rune.name ++ "\nUNLOCKED")
                { x = w * 0.5
                , y = h * 0.65
                , scaleX = 0.00035 * radius
                , scaleY = 0.00035 * radius
                , color =
                    vec3 (40 / 255) (20 / 255) (20 / 255)
                }
                ctx
            ++ Font.view
                "Futura"
                (rune.name ++ "\nUNLOCKED")
                { x = (w * 0.5) - (0.008 * radius)
                , y = h * 0.65
                , scaleX = 0.00035 * radius
                , scaleY = 0.00035 * radius
                , color = Colour.yellow
                }
                ctx
        ]


buttonEntities : Render.Params -> Buttons -> GameType -> Float -> MouseState -> Buttons
buttonEntities renderParams buttons gameType dt mouseState =
    let
        w =
            toFloat renderParams.w

        h =
            toFloat renderParams.h

        textColor =
            vec3 (0 / 255) (0 / 255) (80 / 255)

        bgColor =
            vec3 (244 / 255) (241 / 255) (94 / 255)

        buttonWidth =
            0.12 * max w h

        buttonHeight =
            0.02 * max w h
    in
    Buttons.fromList <|
        List.map (\f -> f dt mouseState buttons)
            (case gameType of
                QuickplayGame ->
                    [ Buttons.entity
                        "continue"
                        { x = 0.5 * w
                        , y = 0.5 * h
                        , width = buttonWidth
                        , height = buttonHeight
                        , btn =
                            TextButton
                                { font = "Futura"
                                , text = "Continue?"
                                , textColor = textColor
                                , bgColor = bgColor
                                , options = [ Buttons.HoverText "Continue!" ]
                                }
                        , disabled = False
                        }
                    ]

                _ ->
                    [ Buttons.entity
                        "playAgain"
                        { x = 0.5 * w
                        , y = 0.5 * h
                        , width = buttonWidth
                        , height = buttonHeight
                        , btn =
                            TextButton
                                { font = "Futura"
                                , text = "Play Again?"
                                , textColor = textColor
                                , bgColor = bgColor
                                , options = [ Buttons.HoverText "Play Again!" ]
                                }
                        , disabled = False
                        }
                    ]
            )
