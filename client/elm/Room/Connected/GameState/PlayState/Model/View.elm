module Model.View exposing (focusImageView, focusTextView, view)

import Animation.State as Animation exposing (animMaxTick)
import Animation.Types exposing (Anim(..))
import Background.View as Background
import Card.State exposing (cardTexture)
import Colour
import Connected.Messages as Connected
import Ease
import Font.State as Font
import Font.Types as Font
import Font.View as Font
import Game.State exposing (contextInit)
import Game.Types as Game exposing (Context, Feedback, Hover(..), HoverSelf)
import GameState.Messages as GameState
import Hand.State exposing (maxHandLength)
import Hand.View as Hand
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, height, style, width)
import Html.Events exposing (onClick)
import Main.Messages as Main
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.Wave as Wave
import PlayState.Messages as PlayState
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
import Room.Messages as Room
import Stack.Types exposing (StackCard)
import Stack.View as Stack
import Texture.State as Texture
import Texture.Types as Texture
import Trail
import Util exposing (interpFloat, px)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Render.Params -> Game.Model -> Texture.Model -> Font.Model -> Html Main.Msg
view { w, h, pixelRatio } { res, hover, focus, entities, passed, feedback, vfx } textures fonts =
    let
        ctx =
            contextInit ( w, h ) res textures fonts Nothing

        ( turnHtml, turnWebGl ) =
            turnView ctx focus passed
    in
    div [ class "clock" ]
        [ WebGL.toHtml [ width <| floor <| toFloat w * pixelRatio, height <| floor <| toFloat h * pixelRatio, class "webgl-canvas" ]
            (List.concat <|
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
                    , Trail.view
                    , Hand.view entities.hand
                    , Hand.otherView entities.otherHand
                    , \_ -> turnWebGl
                    , Hand.millView

                    -- , Background.cursorView
                    , damageWebGl hover
                    , feedbackView feedback
                    ]
            )
        , div [ class "text-focus" ] [ focusTextView ctx focus ]
        , div [ class "clock-life-container" ] (lifeTextView ctx)

        -- , div [ class "clock-damage-container" ] (damageTextView hover ctx)
        , turnHtml
        , goButtonView ctx passed
        ]


focusImageView : Maybe StackCard -> Context -> List WebGL.Entity
focusImageView focus { w, h, anim, radius, textures } =
    case anim of
        Pass _ ->
            []

        _ ->
            case Maybe.join <| Maybe.map (cardTexture textures << .card) focus of
                Just texture ->
                    let
                        color =
                            case focus of
                                Just { owner } ->
                                    case owner of
                                        PlayerA ->
                                            Colour.white

                                        PlayerB ->
                                            Colour.black

                                Nothing ->
                                    Colour.tea
                    in
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.2 * radius) (0.2 * radius) 1
                        , color = color
                        , pos = vec3 (w * 0.5) (h * 0.43) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
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
                (vec2 (w * 0.5 - 0.6 * radius) (h * 0.5 - 0.675 * radius))
                (vec2 shake shake)

        otherPos =
            Math.Vector2.add
                (vec2 (w * 0.5 + 0.6 * radius) (h * 0.5 - 0.75 * radius))
                (vec2 -otherShake -otherShake)
    in
    [ Render.Primitives.fullCircle <|
        uniColourMag ctx
            Colour.white
            1.0
            { scale = 0.15 * radius
            , position = pos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            Colour.black
            lifePercentage
            { scale = 0.15 * radius
            , position = pos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            Colour.black
            1.0
            { scale = 0.15 * radius
            , position = otherPos
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            Colour.white
            otherLifePercentage
            { scale = 0.15 * radius
            , position = otherPos
            , rotation = 0
            }
    ]


focusTextView : Context -> Maybe StackCard -> Html a
focusTextView { anim, radius } focus =
    case anim of
        Mill _ _ ->
            text ""

        Pass _ ->
            text ""

        HandFullPass ->
            text ""

        _ ->
            case focus of
                Nothing ->
                    text ""

                Just { card, owner } ->
                    div
                        [ style "width" (0.7 * radius |> px)
                        , classList [ ( "opponent", owner == PlayerB ) ]
                        ]
                        [ div [ class "title" ] [ text card.name ]
                        , div [ class "desc" ] [ text card.desc ]
                        ]


lifeTextView : Context -> List (Html a)
lifeTextView { radius, model } =
    let
        horizontalOffset =
            0.49 * radius

        verticalOffset =
            0.57 * radius

        textWidth =
            0.2 * radius

        fontSize =
            0.18 * radius
    in
    [ div
        [ class "clock-life"
        , style "right" (horizontalOffset |> px)
        , style "bottom" (verticalOffset - 0.04 * radius |> px)
        , style "font-size" (fontSize |> px)
        , style "transform" "translate(-20%, -10%)"
        , style "width" (textWidth |> px)
        , style "color" "#2d56ca"
        ]
        [ text <| String.fromInt model.life ]
    , div
        [ class "clock-life other"
        , style "left" (horizontalOffset |> px)
        , style "bottom" (verticalOffset + 0.02 * radius |> px)
        , style "font-size" (fontSize |> px)
        , style "transform" "translate(20%, 10%)"
        , style "width" (textWidth |> px)
        , style "color" "#bb3325"
        ]
        [ text <| String.fromInt model.otherLife ]
    ]



-- damageTextView : HoverSelf -> Context -> List (Html a)
-- damageTextView hover { radius, resolving, animDamage } =
--     let
--         hoverDmg =
--             case hover of
--                 HoverHand { dmg } ->
--                     Just dmg
--
--                 HoverStack { dmg } ->
--                     Just dmg
--
--                 NoHover ->
--                     Nothing
--
--         ( damage, otherDamage ) =
--             case hoverDmg of
--                 Just dmg ->
--                     if resolving then
--                         animDamage
--
--                     else
--                         let
--                             ( dmgA, dmgB ) =
--                                 dmg
--                         in
--                         ( toFloat dmgA, toFloat dmgB )
--
--                 Nothing ->
--                     animDamage
--
--         damageToString : Float -> String
--         damageToString d =
--             if d > 0 then
--                 "+" ++ String.fromFloat d
--
--             else
--                 String.fromFloat d
--
--         damageToCssColour : Float -> String
--         damageToCssColour d =
--             if d > 0 then
--                 "#45f273"
--
--             else
--                 "#ff3232"
--     in
--     List.concat
--         [ if damage /= 0 then
--             [ div
--                 [ class "clock-damage"
--                 , style "right" (0.28 * radius |> px)
--                 , style "bottom" (0.75 * radius |> px)
--                 , style "font-size" (0.2 * radius |> px)
--                 , style "color" (damageToCssColour damage)
--                 ]
--                 [ text <| damageToString damage ]
--             ]
--
--           else
--             []
--         , if otherDamage /= 0 then
--             [ div
--                 [ class "clock-damage"
--                 , style "left" (0.22 * radius |> px)
--                 , style "bottom" (0.75 * radius |> px)
--                 , style "font-size" (0.2 * radius |> px)
--                 , style "color" (damageToCssColour otherDamage)
--                 ]
--                 [ text <| damageToString otherDamage ]
--             ]
--
--           else
--             []
--         ]


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


goButtonView : Context -> Bool -> Html Main.Msg
goButtonView { model, radius, resolving } passed =
    let
        handFull =
            List.length model.hand == maxHandLength

        yourTurn =
            model.turn == PlayerA

        isDisabled =
            handFull || not yourTurn || passed || resolving

        horizontalOffset =
            0.64 * radius

        verticalOffset =
            0.67 * radius

        buttonSize =
            0.23 * radius

        fontSize =
            0.12 * radius

        playMsg =
            Main.RoomMsg
                << Room.ConnectedMsg
                << Connected.GameStateMsg
                << GameState.PlayStateMsg
                << PlayState.PlayingOnly

        clickHandler =
            if not isDisabled then
                [ onClick <| playMsg <| PlayState.TurnOnly PlayState.EndTurn
                ]

            else if handFull && not resolving then
                [ onClick <| playMsg PlayState.IllegalPass
                ]

            else
                []
    in
    button
        ([ classList [ ( "clock-go", True ), ( "clock-go--disabled", isDisabled ) ]
         , style "left" ("calc(50% + " ++ (horizontalOffset |> px) ++ ")")
         , style "top" ("calc(50% + " ++ (verticalOffset |> px) ++ ")")
         , style "width" (buttonSize |> px)
         , style "height" (buttonSize |> px)
         , style "font-size" (fontSize |> px)
         ]
            ++ clickHandler
        )
        [ text "GO" ]


turnView : Context -> Maybe StackCard -> Bool -> ( Html Main.Msg, List WebGL.Entity )
turnView { anim, model, tick, w, h, radius, textures } focus passed =
    let
        size =
            radius * 3
    in
    case ( anim, focus, passed ) of
        ( Mill _ _, _, _ ) ->
            ( div [] [], [] )

        ( NullAnim, Nothing, False ) ->
            case model.turn of
                PlayerA ->
                    ( div [] []
                    , Texture.with textures "yourTurn.png" <|
                        \texture ->
                            [ Render.Primitives.quad Render.Shaders.fragment
                                { rotation = makeRotate pi (vec3 0 0 1)
                                , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                                , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                                , pos = vec3 (w * 0.5 - 0.003 * size) (h * 0.5) 0
                                , worldRot = makeRotate 0 (vec3 0 0 1)
                                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                                , texture = texture
                                }
                            , Render.Primitives.quad Render.Shaders.fragment
                                { rotation = makeRotate pi (vec3 0 0 1)
                                , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                                , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , pos = vec3 (w * 0.5) (h * 0.5) 0
                                , worldRot = makeRotate 0 (vec3 0 0 1)
                                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                                , texture = texture
                                }
                            ]
                    )

                PlayerB ->
                    ( div [] []
                    , Texture.with textures "theirTurn.png" <|
                        \texture ->
                            [ Render.Primitives.quad Render.Shaders.fragment
                                { rotation = makeRotate pi (vec3 0 0 1)
                                , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                                , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                                , pos = vec3 (w * 0.5 - 0.003 * size) (h * 0.5) 0
                                , worldRot = makeRotate 0 (vec3 0 0 1)
                                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                                , texture = texture
                                }
                            , Render.Primitives.quad Render.Shaders.fragment
                                { rotation = makeRotate pi (vec3 0 0 1)
                                , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                                , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , pos = vec3 (w * 0.5) (h * 0.5) 0
                                , worldRot = makeRotate 0 (vec3 0 0 1)
                                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                                , texture = texture
                                }
                            ]
                    )

        ( Pass _, _, _ ) ->
            ( div [] []
            , Texture.with textures "pass.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                        , color = vec3 (20 / 255) (20 / 255) (20 / 255)
                        , pos = vec3 (w * 0.5 - 0.003 * size) (h * 0.5) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    , Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.15 * size + sin (tick * 0.005)) (0.15 * size + sin (tick * 0.007)) 1
                        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    ]
            )

        ( HandFullPass, _, _ ) ->
            ( div [ class "pass-status" ] [ text "HAND FULL" ], [] )

        _ ->
            ( div [] [], [] )


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
