module Endgame.View exposing (animView, buttonEntities, view)

import Animation.Types exposing (Anim(..))
import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Buttons.View as Buttons
import Ease
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import GameType exposing (GameType(..))
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Stats as Stats exposing (StatChange)
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


view : Render.Params -> Assets.Model -> Maybe WhichPlayer -> Bool -> Maybe StatChange -> Buttons -> List WebGL.Entity
view { w, h } assets winner resolving mStats buttons =
    let
        ctx =
            bareContextInit ( w, h ) assets NoMouse
    in
    if resolving then
        []

    else
        List.concat
            [ backgroundView { ctx | anim = GameEnd winner, progress = 1 }
            , xpView mStats ctx
            , Buttons.view buttons ctx
            ]


xpView : Maybe StatChange -> Context -> List WebGL.Entity
xpView mStats ctx =
    let
        { camera2d, ortho, w, h, radius } =
            ctx
    in
    case mStats of
        Just { initialXp, tick, finalXp } ->
            let
                start =
                    currentXp - Stats.levelAt currentXp

                end =
                    Stats.nextLevelAt currentXp

                startTick =
                    800

                maxTick =
                    1000

                progress =
                    Ease.outQuad (max 0 (min 1 ((tick - startTick) / maxTick)))

                currentXp =
                    interpFloat progress initialXp finalXp

                color =
                    vec3 (244 / 255) (241 / 255) (94 / 255)
            in
            Render.Primitives.donut
                { rotation = makeRotate 0 (vec3 0 0 1)
                , scale = makeScale3 -(0.5 * radius) (0.5 * radius) 1
                , color = color
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , mag = 1 - (start / end)
                }
                :: Font.view
                    "Futura"
                    ("Level " ++ String.fromInt (Stats.levelFromExperience currentXp))
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.0005 * radius
                    , scaleY = 0.0005 * radius
                    , color = color
                    }
                    ctx

        Nothing ->
            []


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
                        , y = 0.45 * h
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
                    , Buttons.entity
                        "watchReplay"
                        { x = 0.5 * w
                        , y = 0.55 * h
                        , width = buttonWidth
                        , height = buttonHeight
                        , btn =
                            TextButton
                                { font = "Futura"
                                , text = "Watch Replay?"
                                , textColor = textColor
                                , bgColor = bgColor
                                , options = [ Buttons.HoverText "Watch Replay!" ]
                                }
                        , disabled = False
                        }
                    ]

                _ ->
                    [ Buttons.entity
                        "playAgain"
                        { x = 0.5 * w
                        , y = 0.45 * h
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
                    , Buttons.entity
                        "watchReplay"
                        { x = 0.5 * w
                        , y = 0.55 * h
                        , width = buttonWidth
                        , height = buttonHeight
                        , btn =
                            TextButton
                                { font = "Futura"
                                , text = "Watch Replay?"
                                , textColor = textColor
                                , bgColor = bgColor
                                , options = [ Buttons.HoverText "Watch Replay!" ]
                                }
                        , disabled = False
                        }
                    ]
            )
