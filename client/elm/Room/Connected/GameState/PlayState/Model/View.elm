module Model.View exposing (focusImageView, focusTextView, view)

import Animation.State as Animation exposing (animMaxTick)
import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Card.View as Card
import Chat.Types as Chat
import Chat.View as Chat
import Colour
import Ease
import Endgame.View as Endgame
import Font.State as Font
import Font.Types as Font
import Font.View as Font
import Game.State exposing (contextInit)
import Game.Types as Game exposing (Context, Focus(..), PlayerEntity)
import Hand.View as Hand
import Holding.Types exposing (Holding(..))
import Holding.View as Holding
import Hover exposing (Hover(..), HoverDamage(..), HoverSelf)
import Math.Matrix4 exposing (makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Model.Wave as Wave
import Mouse exposing (MouseState(..))
import Quaternion
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
import Stack.Entities exposing (wheelZ)
import Stack.View as Stack
import Texture.State as Texture
import TimeLimit
import Tutorial
import Util exposing (interpFloat)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Render.Params -> Game.Model -> Chat.Model -> Assets.Model -> List WebGL.Entity
view { w, h } game chat assets =
    let
        { res, hover, focus, entities, passed, vfx, buttons, holding, timeLeft } =
            game

        ctx =
            contextInit ( w, h ) res assets NoMouse vfx
    in
    List.concat <|
        List.map ((|>) ctx)
            [ Background.radialView vfx
            , tutorialArrowView game.tutorial
            , lifeOrbView entities.players
            , Wave.view
            , Stack.wheelBgView entities.wheel
            , Stack.view entities.stack
            , focusImageView (vec3 0 0.15 0) focus
            , Hand.view entities.hand
            , Hand.otherView entities.otherHand
            , Hand.millView
            , damageView hover holding
            , turnView focus passed game.tutorial timeLeft
            , focusTextView (vec2 0 0) focus
            , Buttons.view buttons
            , Chat.notifyView chat buttons
            , timeLeftView timeLeft
            , Endgame.animView
            , Holding.view holding
            ]


focusImageView : Vec3 -> Focus -> Context -> List WebGL.Entity
focusImageView originVec focus ({ anim, tick } as ctx) =
    case anim of
        Mill _ _ _ ->
            []

        Pass _ ->
            []

        HandFullPass ->
            []

        Timeout ->
            []

        _ ->
            case focus of
                FocusCard { card, owner } ->
                    let
                        shake =
                            0.01 * (Animation.animShake anim PlayerA tick + Animation.animShake anim PlayerB tick)

                        entity =
                            { rotation = Quaternion.identity
                            , scale = vec3 0.13 0.13 0.13
                            , position = Math.Vector3.add originVec (vec3 shake shake shake)
                            , card = card
                            , owner = owner
                            , revealed = False
                            }
                    in
                    Card.view ctx entity

                _ ->
                    []


lifeOrbView : List PlayerEntity -> Context -> List WebGL.Entity
lifeOrbView entities ({ radius, model, anim, animDamage, tick } as ctx) =
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

        textScale =
            0.00035 * radius

        life =
            floor <| toFloat model.maxLife * finalLifePercentage

        otherLife =
            floor <| toFloat model.otherMaxLife * finalOtherLifePercentage

        eachView : PlayerEntity -> List WebGL.Entity
        eachView { which, position, scale } =
            [ Render.Primitives.fullCircle <|
                uniColourMag ctx
                    (Colour.background which)
                    1.0
                    { scale = scale
                    , position = position
                    , rotation = 0
                    }
            , Render.Primitives.fullCircle <|
                uniColourMag ctx
                    (Colour.card which)
                    (case which of
                        PlayerA ->
                            lifePercentage

                        PlayerB ->
                            otherLifePercentage
                    )
                    { scale = scale
                    , position = position
                    , rotation = 0
                    }
            ]
                ++ Font.view "Futura"
                    (String.fromInt
                        (case which of
                            PlayerA ->
                                life

                            PlayerB ->
                                otherLife
                        )
                    )
                    { x = Math.Vector2.getX position
                    , y = Math.Vector2.getY position
                    , scaleX = textScale
                    , scaleY = textScale
                    , color = Colour.yellow
                    }
                    ctx
    in
    List.concat <| List.map eachView entities


focusTextView : Vec2 -> Focus -> Context -> List WebGL.Entity
focusTextView originVec focus ({ w, h, anim, model, radius, tick } as ctx) =
    case anim of
        Mill _ _ _ ->
            []

        Pass _ ->
            []

        HandFullPass ->
            []

        Timeout ->
            []

        _ ->
            let
                shake =
                    Animation.animShake anim PlayerA tick + Animation.animShake anim PlayerB tick

                origin =
                    Math.Vector2.toRecord originVec
            in
            case focus of
                FocusCard { card } ->
                    List.concat
                        [ Font.view "Futura"
                            card.name
                            { x = origin.x + 0.5 * w + shake
                            , y = origin.y + 0.5 * h + radius * 0.15 + shake
                            , scaleX = 0.00025 * radius
                            , scaleY = 0.00025 * radius
                            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
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

                FocusPlayer which ->
                    let
                        life =
                            case which of
                                PlayerA ->
                                    model.life

                                PlayerB ->
                                    model.otherLife

                        cardsLeft =
                            case which of
                                PlayerA ->
                                    model.deck

                                PlayerB ->
                                    model.otherDeck

                        text =
                            String.join "\n"
                                [ "LIFE: " ++ String.fromInt life
                                , "DECK: " ++ String.fromInt cardsLeft
                                ]
                    in
                    Font.view "Futura"
                        text
                        { x = origin.x + 0.5 * w + shake
                        , y = origin.y + 0.5 * h - radius * 0.05 + shake
                        , scaleX = 0.00025 * radius
                        , scaleY = 0.00025 * radius
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        }
                        ctx

                _ ->
                    []


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
                animDamageToHoverDamage animDamage

            else
                hoverDmg

        animDamageToHoverDamage : ( Float, Float ) -> ( HoverDamage, HoverDamage )
        animDamageToHoverDamage ( a, b ) =
            ( HoverDamage <| floor a, HoverDamage <| floor b )

        damageToColour : HoverDamage -> Vec3
        damageToColour dmg =
            case dmg of
                HoverDamage d ->
                    if d > 0 then
                        vec3 0 1 0

                    else
                        vec3 1 0 0

                HoverDamageUncertain ->
                    vec3 1 0 0

        damageToString : HoverDamage -> String
        damageToString dmg =
            case dmg of
                HoverDamage d ->
                    if d > 0 then
                        "+" ++ String.fromInt d

                    else
                        String.fromInt d

                HoverDamageUncertain ->
                    "?"

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
        [ if damage /= HoverDamage 0 then
            Font.view "Futura"
                (damageToString damage)
                { x = 0.5 * w + xOffset
                , y = 0.5 * h - yOffset
                , scaleX = scale
                , scaleY = scale
                , color = damageToColour damage
                }
                ctx

          else
            []
        , if otherDamage /= HoverDamage 0 then
            Font.view "Futura"
                (damageToString otherDamage)
                { x = 0.5 * w - xOffset
                , y = 0.5 * h - yOffset
                , scaleX = scale
                , scaleY = scale
                , color = damageToColour otherDamage
                }
                ctx

          else
            []
        ]


turnView : Focus -> Bool -> Tutorial.Model -> Maybe Float -> Context -> List WebGL.Entity
turnView focus passed tutorial timeLeft ctx =
    let
        { anim, model, tick, w, h, radius } =
            ctx

        size =
            radius * 3

        timeLeftProgress =
            Maybe.withDefault 0 (Maybe.map TimeLimit.progress timeLeft)
    in
    case ( anim, focus, passed ) of
        ( Mill _ _ _, _, _ ) ->
            []

        ( NullAnim, NoFocus, False ) ->
            if timeLeftProgress > 0 then
                case timeLeft of
                    Just t ->
                        let
                            str =
                                String.fromInt (ceiling (max 0 t / 1000))
                        in
                        List.concat
                            [ Font.view
                                "Futura"
                                str
                                { x = w * 0.5 - 0.003 * size
                                , y = h * 0.5
                                , scaleX = 0.0003 * size + 0.003 * sin (tick * 0.005)
                                , scaleY = 0.0003 * size + 0.003 * sin (tick * 0.007)
                                , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                                }
                                ctx
                            , Font.view
                                "Futura"
                                str
                                { x = w * 0.5
                                , y = h * 0.5
                                , scaleX = 0.0003 * size + 0.003 * sin (tick * 0.005)
                                , scaleY = 0.0003 * size + 0.003 * sin (tick * 0.007)
                                , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                                }
                                ctx
                            ]

                    Nothing ->
                        []

            else
                case model.turn of
                    PlayerA ->
                        if Tutorial.isActive tutorial then
                            tutorialView tutorial ctx

                        else
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

        ( Timeout, _, _ ) ->
            List.concat
                [ Font.view
                    "Futura"
                    "TIMEOUT!"
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.5
                    , scaleX = 0.00015 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.00015 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    "TIMEOUT!"
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.00015 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.00015 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    }
                    ctx
                ]

        _ ->
            []


tutorialView : Tutorial.Model -> Context -> List WebGL.Entity
tutorialView tutorial ctx =
    let
        { w, h, radius, tick } =
            ctx

        size =
            radius * 3
    in
    case tutorial.step of
        Just Tutorial.DragACard ->
            List.concat
                [ Font.view
                    "Futura"
                    "DRAG A CARD"
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    "DRAG A CARD"
                    { x = w * 0.5
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    }
                    ctx
                ]

        Just Tutorial.PressGo ->
            List.concat
                [ Font.view
                    "Futura"
                    "PRESS GO"
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.5
                    , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
                    , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
                    , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Futura"
                    "PRESS GO"
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


tutorialArrowView : Tutorial.Model -> Context -> List WebGL.Entity
tutorialArrowView tutorial ctx =
    let
        { camera3d, perspective, textures, radius } =
            ctx

        scale =
            0.0007 * radius
    in
    case tutorial.step of
        Just Tutorial.DragACard ->
            List.concat
                [ Texture.with textures "arrow.png" <|
                    \texture ->
                        [ Render.Primitives.quad Render.Shaders.fragment
                            { texture = texture
                            , rotation = Quaternion.makeRotate Quaternion.identity
                            , scale = makeScale3 scale scale 1
                            , color = Colour.white
                            , pos = vec3 0 0 0
                            , perspective = perspective
                            , camera = camera3d
                            }
                        ]
                ]

        Just Tutorial.PressGo ->
            List.concat
                [ Texture.with textures "arrow.png" <|
                    \texture ->
                        [ Render.Primitives.quad Render.Shaders.fragment
                            { texture = texture
                            , rotation = Quaternion.makeRotate <| Quaternion.zRotation (0.75 * pi)
                            , scale = makeScale3 scale scale 1
                            , color = Colour.white
                            , pos = vec3 0 0 0
                            , perspective = perspective
                            , camera = camera3d
                            }
                        ]
                ]

        _ ->
            []


timeLeftView : Maybe Float -> Context -> List WebGL.Entity
timeLeftView timeLeft ({ perspective, camera3d } as ctx) =
    case timeLeft of
        Just t ->
            let
                timeLimitProgress =
                    TimeLimit.progress t
            in
            if timeLimitProgress > 0 then
                [ Render.Primitives.quad Render.Shaders.donutFragment <|
                    { rotation = Quaternion.makeRotate <| Quaternion.zRotation pi
                    , scale = makeScale3 0.42 0.42 1
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    , pos = vec3 0 0 (0.5 * wheelZ ctx)
                    , perspective = perspective
                    , camera = camera3d
                    , mag = timeLimitProgress
                    , thickness = 0.1
                    }
                ]

            else
                []

        Nothing ->
            []
