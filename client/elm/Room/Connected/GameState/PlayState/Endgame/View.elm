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
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


animView : Context -> List WebGL.Entity
animView ({ camera2d, ortho, w, h, radius, anim, progress } as ctx) =
    case anim of
        GameEnd winner ->
            let
                ( text, backgroundColor ) =
                    case winner of
                        Just PlayerA ->
                            ( "VICTORY", vec3 (30 / 255) (200 / 255) (30 / 255) )

                        Just PlayerB ->
                            ( "DEFEAT", vec3 (200 / 255) (30 / 255) (30 / 255) )

                        Nothing ->
                            ( "DRAW", vec3 (255 / 255) (255 / 255) (255 / 255) )

                color =
                    vec3 (244 / 255) (241 / 255) (94 / 255)

                shadowOffsetX =
                    0.009 * radius

                scale =
                    0.0008 * radius
            in
            List.concat
                [ [ Render.Primitives.quad Render.Shaders.matte
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 w h 1
                        , color = backgroundColor
                        , alpha = 0.8 * Ease.outCubic progress
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        }
                  ]
                , Font.view
                    "Futura"
                    text
                    { x = w * 0.5 - shadowOffsetX
                    , y = h * 0.4
                    , scaleX = scale * Ease.outBounce (progress * progress)
                    , scaleY = scale * Ease.outBounce (progress * progress)
                    , color = vec3 (40 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    text
                    { x = w * 0.5
                    , y = h * 0.4
                    , scaleX = scale * Ease.outBounce progress
                    , scaleY = scale * Ease.outBounce progress
                    , color = color
                    }
                    ctx
                ]

        _ ->
            []


view : Render.Params -> Assets.Model -> Maybe WhichPlayer -> Bool -> Buttons -> List WebGL.Entity
view { w, h } assets winner resolving buttons =
    let
        ctx =
            bareContextInit ( w, h ) assets NoMouse
    in
    if resolving then
        []

    else
        List.concat
            [ animView { ctx | anim = GameEnd winner, progress = 1 }
            , Buttons.view buttons ctx
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
                        , y = 0.55 * h
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
                        , y = 0.55 * h
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
                        , y = 0.65 * h
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
