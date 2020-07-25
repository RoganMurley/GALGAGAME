module Model.View exposing (focusImageView, focusTextView, view)

-- import Trail

import Animation.State as Animation exposing (animMaxTick)
import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Card.State exposing (cardTexture)
import Colour
import Ease
import Endgame.WebGL as Endgame
import Font.State as Font
import Font.Types as Font
import Font.View as Font
import Game.State exposing (contextInit)
import Game.Types as Game exposing (ButtonEntity, Context, Feedback)
import Hand.View as Hand
import Hover exposing (Hover(..), HoverSelf)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.Wave as Wave
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
import Stack.Types exposing (StackCard)
import Stack.View as Stack
import Util exposing (interpFloat)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Render.Params -> Game.Model -> Assets.Model -> List WebGL.Entity
view { w, h } { res, hover, focus, entities, passed, feedback, vfx } assets =
    let
        ctx =
            contextInit ( w, h ) res assets Nothing
    in
    List.concat <|
        List.map ((|>) ctx)
            [ Background.radialView vfx
            , Wave.view

            -- , Background.ornateView
            , lifeOrbView
            , passView

            -- , Background.stainView focus
            -- , Background.ringView
            , Stack.view entities.stack
            , focusImageView focus

            -- , Trail.view
            , Hand.view entities.hand
            , Hand.otherView entities.otherHand
            , Hand.millView

            -- , Background.cursorView
            , damageWebGl hover
            , turnView focus passed
            , focusTextView focus
            , buttonsView entities.buttons
            , Endgame.animView
            , feedbackView feedback
            ]


focusImageView : Maybe StackCard -> Context -> List WebGL.Entity
focusImageView focus { ortho, camera, w, h, anim, radius, textures } =
    case anim of
        Pass _ ->
            []

        _ ->
            case Maybe.join <| Maybe.map (cardTexture textures << .card) focus of
                Just texture ->
                    let
                        color =
                            Colour.white
                    in
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.2 * radius) (0.2 * radius) 1
                        , color = color
                        , pos = vec3 (w * 0.5) (h * 0.43) 0
                        
                        , perspective = ortho
                        , camera = camera
                        , texture = texture
                        }
                    ]

                Nothing ->
                    []


lifeOrbView : Context -> List WebGL.Entity
lifeOrbView ({ w, h, radius, model, anim, animDamage, tick } as ctx) =
    let
        progress =
            Ease.outQuad (tick / animMaxTick anim)

        finalLifePercentage =
            toFloat model.life / 50

        finalOtherLifePercentage =
            toFloat model.otherLife / 50

        ( lifeChange, otherLifeChange ) =
            animDamage

        lifePercentage =
            interpFloat
                progress
                (finalLifePercentage - (lifeChange / 50) * finalLifePercentage)
                finalLifePercentage

        otherLifePercentage =
            interpFloat
                progress
                (finalOtherLifePercentage - (otherLifeChange / 50) * finalOtherLifePercentage)
                finalOtherLifePercentage

        shake =
            Animation.animShake anim PlayerA tick

        otherShake =
            Animation.animShake anim PlayerB tick

        pos =
            Math.Vector2.add
                (vec2 (w * 0.5 - 0.8 * radius) (h * 0.5 - 0.675 * radius))
                (vec2 -shake shake)

        otherPos =
            Math.Vector2.add
                (vec2 (w * 0.5 + 0.8 * radius) (h * 0.5 - 0.675 * radius))
                (vec2 otherShake otherShake)

        textScale =
            0.085

        life =
            floor <| 50 * lifePercentage

        otherLife =
            floor <| 50 * otherLifePercentage
    in
    [ Render.Primitives.fullCircle <|
        uniColourMag ctx
            (Colour.background PlayerA)
            1.0
            { scale = 0.15 * radius
            , position = pos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            (Colour.card PlayerA)
            lifePercentage
            { scale = 0.15 * radius
            , position = pos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            (Colour.background PlayerB)
            1.0
            { scale = 0.15 * radius
            , position = otherPos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            (Colour.card PlayerB)
            otherLifePercentage
            { scale = 0.15 * radius
            , position = otherPos
            , rotation = 0
            }
    ]
        ++ List.concat
            [ Font.view "Futura"
                (String.fromInt life)
                { x = Math.Vector2.getX pos
                , y = Math.Vector2.getY pos
                , scaleX = textScale
                , scaleY = textScale
                , color = Colour.yellow
                }
                ctx
            , Font.view "Futura"
                (String.fromInt otherLife)
                { x = Math.Vector2.getX otherPos
                , y = Math.Vector2.getY otherPos
                , scaleX = textScale
                , scaleY = textScale
                , color = Colour.yellow
                }
                ctx
            ]


focusTextView : Maybe StackCard -> Context -> List WebGL.Entity
focusTextView focus ({ w, h, anim, radius } as ctx) =
    case anim of
        Mill _ _ ->
            []

        Pass _ ->
            []

        HandFullPass ->
            []

        _ ->
            case focus of
                Nothing ->
                    []

                Just { card } ->
                    List.concat
                        [ Font.view "Futura"
                            card.name
                            { x = 0.5 * w
                            , y = 0.5 * h + radius * 0.1
                            , scaleX = 0.00025 * radius
                            , scaleY = 0.00025 * radius
                            , color = Colour.white
                            }
                            ctx
                        , Font.view "Futura"
                            card.desc
                            { x = 0.5 * w
                            , y = 0.5 * h + radius * 0.3
                            , scaleX = 0.00012 * radius
                            , scaleY = 0.00012 * radius
                            , color = Colour.white
                            }
                            ctx
                        ]


damageWebGl : HoverSelf -> Context -> List WebGL.Entity
damageWebGl hover ({ w, h, radius, resolving, animDamage } as ctx) =
    let
        hoverDmg =
            case hover of
                HoverHand { dmg } ->
                    Just dmg

                HoverStack { dmg } ->
                    Just dmg

                NoHover ->
                    Nothing

        ( damage, otherDamage ) =
            case hoverDmg of
                Just dmg ->
                    if resolving then
                        animDamage

                    else
                        let
                            ( dmgA, dmgB ) =
                                dmg
                        in
                        ( toFloat dmgA, toFloat dmgB )

                Nothing ->
                    animDamage

        damageToColour : Float -> Vec3
        damageToColour d =
            if d > 0 then
                vec3 0 1 0

            else
                vec3 1 0 0

        damageToString : Float -> String
        damageToString d =
            if d > 0 then
                "+" ++ String.fromFloat d

            else
                String.fromFloat d

        scale =
            0.07

        xOffset =
            0.4 * radius

        yOffset =
            0.9 * radius
    in
    List.concat
        [ if damage /= 0 then
            Font.view "Futura"
                (damageToString damage)
                { x = 0.5 * w - xOffset
                , y = 0.5 * h - yOffset
                , scaleX = scale
                , scaleY = scale
                , color = damageToColour damage
                }
                ctx

          else
            []
        , if otherDamage /= 0 then
            Font.view "Futura"
                (damageToString otherDamage)
                { x = 0.5 * w + xOffset
                , y = 0.5 * h - yOffset
                , scaleX = scale
                , scaleY = scale
                , color = damageToColour otherDamage
                }
                ctx

          else
            []
        ]


buttonsView : List ButtonEntity -> Context -> List WebGL.Entity
buttonsView buttons ctx =
    let
        buttonView : ButtonEntity -> List WebGL.Entity
        buttonView { font, text, position, scale, disabled, hover } =
            let
                textColor =
                    if disabled then
                        vec3 (100 / 255) (100 / 255) (50 / 255)

                    else
                        vec3 (244 / 255) (241 / 255) (94 / 255)

                backgroundColor =
                    if disabled then
                        vec3 (62 / 255) (62 / 255) (62 / 255)

                    else if hover then
                        vec3 (160 / 255) (160 / 255) (160 / 255)

                    else
                        vec3 (122 / 255) (122 / 255) (122 / 255)
            in
            List.concat
                [ [ Render.Primitives.fullCircle <|
                        uniColourMag ctx
                            backgroundColor
                            1
                            { scale = 0.12 * ctx.radius
                            , position = position
                            , rotation = 0
                            }
                  ]
                , Font.view
                    font
                    text
                    { x = Math.Vector2.getX position
                    , y = Math.Vector2.getY position
                    , scaleX = scale * 0.0016
                    , scaleY = scale * 0.0016
                    , color = textColor
                    }
                    ctx
                ]
    in
    List.concat <|
        List.map buttonView buttons


turnView : Maybe StackCard -> Bool -> Context -> List WebGL.Entity
turnView focus passed ctx =
    let
        { anim, model, tick, w, h, radius } =
            ctx

        size =
            radius * 3
    in
    case ( anim, focus, passed ) of
        ( Mill _ _, _, _ ) ->
            []

        ( NullAnim, Nothing, False ) ->
            case model.turn of
                PlayerA ->
                    List.concat
                        [ Font.view
                            "Futura"
                            "YOUR TURN"
                            { x = w * 0.5 - 0.003 * size
                            , y = h * 0.5
                            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                            , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                            }
                            ctx
                        , Font.view
                            "Futura"
                            "YOUR TURN"
                            { x = w * 0.5
                            , y = h * 0.5
                            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                            }
                            ctx
                        ]

                PlayerB ->
                    List.concat
                        [ Font.view
                            "Futura"
                            "THEIR TURN"
                            { x = w * 0.5 - 0.003 * size
                            , y = h * 0.5
                            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                            , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                            }
                            ctx
                        , Font.view
                            "Futura"
                            "THEIR TURN"
                            { x = w * 0.5
                            , y = h * 0.5
                            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                            }
                            ctx
                        ]

        ( Pass _, _, _ ) ->
            List.concat
                [ Font.view
                    "Futura"
                    "PASS"
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    "PASS"
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    }
                    ctx
                ]

        ( HandFullPass, _, _ ) ->
            List.concat
                [ Font.view
                    "Futura"
                    "HAND FULL"
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    "HAND FULL"
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    }
                    ctx
                ]

        _ ->
            []


passView : Context -> List WebGL.Entity
passView ({ anim, w, h, radius } as ctx) =
    case anim of
        Pass which ->
            [ Render.Primitives.fullCircle <|
                uniColourMag ctx
                    (Colour.focusBackground which)
                    1.0
                    { scale = 0.66 * radius
                    , position = vec2 (w * 0.5) (h * 0.5)
                    , rotation = 0
                    }
            ]

        HandFullPass ->
            [ Render.Primitives.fullCircle <|
                uniColourMag ctx
                    (Colour.focusBackground PlayerA)
                    1.0
                    { scale = 0.66 * radius
                    , position = vec2 (w * 0.5) (h * 0.5)
                    , rotation = 0
                    }
            ]

        _ ->
            []


feedbackView : List Feedback -> Context -> List WebGL.Entity
feedbackView feedback ctx =
    List.map
        (\f ->
            let
                alpha =
                    Ease.inQuint (f.progress / 1000)

                scale =
                    ctx.radius * 0.0005 * (1000 - f.progress)
            in
            Render.Primitives.circle <|
                uniColourMag ctx
                    Colour.white
                    alpha
                    { scale = scale
                    , position = f.pos
                    , rotation = 0
                    }
        )
        feedback
