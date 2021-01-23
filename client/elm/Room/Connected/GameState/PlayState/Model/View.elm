module Model.View exposing (focusImageView, focusTextView, view)

import Animation.State as Animation exposing (animMaxTick)
import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Card.State exposing (cardTexture)
import Colour
import Ease
import Endgame.WebGL as Endgame
import Font.State as Font
import Font.Types as Font
import Font.View as Font
import Game.State exposing (contextInit)
import Game.Types as Game exposing (Context, Feedback)
import Hand.View as Hand
import Holding.Types exposing (Holding(..))
import Holding.View as Holding
import Hover exposing (Hover(..), HoverSelf)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
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
view { w, h } game assets =
    let
        { res, hover, focus, entities, passed, feedback, vfx, buttons, holding } =
            game

        ctx =
            contextInit ( w, h ) res assets Nothing
    in
    List.concat <|
        List.map ((|>) ctx)
            [ Background.radialView vfx
            , lifeOrbView
            , Wave.view
            , Stack.wheelBgView entities.wheel
            , Stack.view entities.stack
            , focusImageView (vec2 0 0) focus
            , Hand.view entities.hand
            , Hand.otherView entities.otherHand
            , Hand.millView
            , damageView hover holding
            , turnView focus passed
            , focusTextView (vec2 0 0) focus
            , Buttons.view buttons
            , Endgame.animView
            , feedbackView feedback
            , Holding.view holding
            ]


focusImageView : Vec2 -> Maybe StackCard -> Context -> List WebGL.Entity
focusImageView originVec focus { tick, ortho, camera2d, w, h, anim, radius, textures } =
    case anim of
        Mill _ _ ->
            []

        Pass _ ->
            []

        HandFullPass ->
            []

        _ ->
            case Maybe.join <| Maybe.map (cardTexture textures << .card) focus of
                Just texture ->
                    let
                        color =
                            Colour.white

                        shake =
                            Animation.animShake anim PlayerA tick + Animation.animShake anim PlayerB tick

                        origin =
                            Math.Vector2.toRecord originVec
                    in
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.2 * radius) (0.2 * radius) 1
                        , color = color
                        , pos = vec3 (origin.x + w * 0.5 + shake) (origin.y + h * 0.43 + shake) 0
                        , perspective = ortho
                        , camera = camera2d
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
            toFloat model.life / toFloat model.maxLife

        finalOtherLifePercentage =
            toFloat model.otherLife / toFloat model.otherMaxLife

        ( lifeChange, otherLifeChange ) =
            animDamage

        lifePercentage =
            interpFloat
                progress
                (finalLifePercentage - (lifeChange / toFloat model.maxLife) * finalLifePercentage)
                finalLifePercentage

        otherLifePercentage =
            interpFloat
                progress
                (finalOtherLifePercentage - (otherLifeChange / toFloat model.otherMaxLife) * finalOtherLifePercentage)
                finalOtherLifePercentage

        shake =
            Animation.animShake anim PlayerA tick

        otherShake =
            Animation.animShake anim PlayerB tick

        ( xOffset, yOffset ) =
            ( 0.65 * radius, 0.875 * radius )

        pos =
            Math.Vector2.add
                (vec2 (w * 0.5 - xOffset) (h * 0.5 - yOffset))
                (vec2 -shake shake)

        otherPos =
            Math.Vector2.add
                (vec2 (w * 0.5 + xOffset) (h * 0.5 - yOffset))
                (vec2 otherShake otherShake)

        textScale =
            0.00035 * radius

        life =
            floor <| toFloat model.maxLife * finalLifePercentage

        otherLife =
            floor <| toFloat model.otherMaxLife * finalOtherLifePercentage
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


focusTextView : Vec2 -> Maybe StackCard -> Context -> List WebGL.Entity
focusTextView originVec focus ({ w, h, anim, radius, tick } as ctx) =
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
                    let
                        shake =
                            Animation.animShake anim PlayerA tick + Animation.animShake anim PlayerB tick

                        origin =
                            Math.Vector2.toRecord originVec
                    in
                    List.concat
                        [ Font.view "Futura"
                            card.name
                            { x = origin.x + 0.5 * w + shake
                            , y = origin.y + 0.5 * h + radius * 0.15 + shake
                            , scaleX = 0.00025 * radius
                            , scaleY = 0.00025 * radius
                            , color = Colour.white
                            }
                            ctx
                        , Font.view "Futura"
                            card.desc
                            { x = origin.x + 0.5 * w + shake
                            , y = origin.y + 0.5 * h + radius * 0.3 + shake
                            , scaleX = 0.00012 * radius
                            , scaleY = 0.00012 * radius
                            , color = Colour.white
                            }
                            ctx
                        ]


damageView : HoverSelf -> Holding -> Context -> List WebGL.Entity
damageView hover holding ({ w, h, radius, resolving, animDamage, tick, anim } as ctx) =
    let
        hoverDmg =
            case holding of
                NoHolding ->
                    Hover.getDmg hover

                Holding { dmg } ->
                    dmg

        ( damage, otherDamage ) =
            if resolving then
                animDamage

            else
                let
                    ( dmgA, dmgB ) =
                        hoverDmg
                in
                ( toFloat dmgA, toFloat dmgB )

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

        progress =
            if resolving then
                Ease.outElastic <| tick / animMaxTick anim

            else
                0

        scale =
            radius * 0.0003 * (1 + 0.05 * progress)

        xOffset =
            0.3 * radius

        yOffset =
            0.95 * radius
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
