module Clock.Hand exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (animToResTickMax, uniforms)
import Clock.Types exposing (ClockParams, GameEntity)
import Ease
import Hand.Types exposing (Hand)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (floatInterp, interp, to3d)


cardDimensions : ClockParams -> ( Float, Float, Float )
cardDimensions { w, h, radius } =
    ( 0.1 * radius, 0.1 * radius, 35.0 )


origin : ClockParams -> WhichPlayer -> Int -> Vec3
origin { w, h, radius } which count =
    let
        ( width, height, spacing ) =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        x =
            w / 2 - 0.5 * (width + spacing) * (toFloat <| count - 1)

        y =
            case which of
                PlayerA ->
                    h - height

                PlayerB ->
                    height
    in
        vec3 x y 0


rotation : WhichPlayer -> Int -> Int -> Float
rotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
        case which of
            PlayerA ->
                pi + magnitude

            PlayerB ->
                -magnitude


position : ClockParams -> WhichPlayer -> Int -> Int -> Vec3
position ({ w, h, radius } as params) which index count =
    let
        ( width, height, spacing ) =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        sign =
            case which of
                PlayerA ->
                    1

                PlayerB ->
                    -1

        y =
            let
                i =
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                sign * (abs <| 4 * (toFloat <| ceiling (i - (c * 0.5))))
    in
        Math.Vector3.add
            (origin params which count)
        <|
            Math.Vector3.add
                (vec3 ((toFloat index) * (width + spacing)) 0 0)
            <|
                vec3 0 y 0


handView : ClockParams -> Hand -> List (GameEntity {}) -> Maybe ( Float, Maybe Anim ) -> Texture -> Texture -> List WebGL.Entity
handView ({ w, h, radius } as params) finalHand handEntities resInfo texture noise =
    let
        locals =
            uniforms 0 ( floor w, floor h )

        hand =
            case anim of
                Just (Draw PlayerA) ->
                    List.take (List.length finalHand - 1) finalHand

                otherwise ->
                    finalHand

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Just (Play PlayerA _ index) ->
                    \i ->
                        if i >= index then
                            i + 1
                        else
                            i

                otherwise ->
                    identity

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        n =
            List.length hand

        finalN =
            List.length finalHand

        progress =
            Ease.outQuint <| resTick / maxTick

        ( width, height, spacing ) =
            cardDimensions params

        entity : GameEntity {} -> List WebGL.Entity
        entity { position, rotation } =
            let
                rot =
                    makeRotate rotation <| vec3 0 0 1

                pos =
                    to3d position
            in
                [ Primitives.roundedBox <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (0.7 * width) height 1)
                        (vec3 0.18 0.49 0.62)
                , Primitives.quad Clock.Shaders.fragment <|
                    locals texture
                        pos
                        (makeScale3 width height 1)
                        rot
                        (vec3 1 1 1)
                ]

        mainView : List WebGL.Entity
        mainView =
            List.concat <| List.map entity handEntities

        extraView : List WebGL.Entity
        extraView =
            case anim of
                Just (Overdraw PlayerA card) ->
                    let
                        pos =
                            interp
                                progress
                                (vec3 w h 0)
                                (vec3 (w / 2) (h / 2) 0)

                        rot =
                            makeRotate (floatInterp progress 0 (0.05 * pi)) <|
                                vec3 0 0 1

                        iWidth =
                            floatInterp
                                progress
                                width
                                (width * 4)

                        iHeight =
                            floatInterp
                                progress
                                height
                                (height * 4)
                    in
                        [ Primitives.roundedBoxDisintegrate <|
                            { resolution = vec2 w h
                            , texture = noise
                            , rotation = rot
                            , scale = makeScale3 (0.7 * iWidth) iHeight 1
                            , color = (vec3 0.18 0.49 0.615)
                            , worldPos = pos
                            , worldRot = makeRotate 0 (vec3 0 0 1)
                            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                            , time = Ease.inQuint <| resTick / maxTick
                            }
                        , Primitives.quad Clock.Shaders.disintegrate <|
                            { resolution = vec2 w h
                            , texture = texture
                            , noise = noise
                            , rotation = rot
                            , scale = makeScale3 iWidth iHeight 1
                            , color = vec3 1 1 1
                            , worldPos = pos
                            , worldRot = makeRotate 0 (vec3 0 0 1)
                            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                            , time = Ease.inQuint <| resTick / maxTick
                            }
                        ]

                otherwise ->
                    []
    in
        mainView ++ extraView


otherHandView : ClockParams -> Int -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
otherHandView ({ w, h, radius } as params) finalN resInfo texture =
    let
        locals =
            uniforms 0 ( floor w, floor h )

        n =
            case anim of
                Just (Draw PlayerB) ->
                    finalN - 1

                otherwise ->
                    finalN

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Just (Play PlayerB _ index) ->
                    \i ->
                        if i >= index then
                            i + 1
                        else
                            i

                otherwise ->
                    identity

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        progress =
            Ease.outQuint <| resTick / maxTick

        ( width, height, spacing ) =
            cardDimensions params

        entity : Int -> List WebGL.Entity
        entity finalI =
            let
                i =
                    indexModifier finalI

                pos =
                    interp progress (position params PlayerB i n) (position params PlayerB finalI finalN)

                rot =
                    makeRotate
                        (floatInterp progress (rotation PlayerB i n) (rotation PlayerB finalI finalN))
                        (vec3 0 0 1)
            in
                [ Primitives.roundedBox <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (0.7 * width) height 1)
                        (vec3 0.52 0.1 0.2)
                , Primitives.quad Clock.Shaders.fragment <|
                    locals texture
                        pos
                        (makeScale3 width height 1)
                        rot
                        (vec3 1 1 1)
                ]

        mainView : List WebGL.Entity
        mainView =
            List.concat <| List.map entity (List.range 0 (n - 1))

        extraView : List WebGL.Entity
        extraView =
            case anim of
                Just (Draw PlayerB) ->
                    let
                        pos =
                            interp
                                progress
                                (vec3 w 0 0)
                                (position params PlayerB n (n + 1))

                        rot =
                            makeRotate
                                (floatInterp progress (0.5 * pi) (rotation PlayerB n (n + 1)))
                                (vec3 0 0 1)
                    in
                        [ Primitives.roundedBox <|
                            uniforms 0
                                ( floor w, floor h )
                                texture
                                pos
                                rot
                                (makeScale3 (0.7 * width) height 1)
                                (vec3 0.52 0.1 0.2)
                        , Primitives.quad Clock.Shaders.fragment <|
                            locals texture
                                pos
                                (makeScale3 width height 1)
                                rot
                                (vec3 1 1 1)
                        ]

                Just (Play PlayerB _ i) ->
                    let
                        playProgress =
                            Ease.inQuad <| resTick / maxTick

                        pos =
                            interp
                                playProgress
                                (position params PlayerB i n)
                                (vec3 (w / 2) (h / 2 - radius * 0.62) 0)

                        rot =
                            makeRotate (floatInterp playProgress (rotation PlayerB i n) 0) <|
                                vec3 0 0 1
                    in
                        [ Primitives.roundedBox <|
                            uniforms 0
                                ( floor w, floor h )
                                texture
                                pos
                                rot
                                (makeScale3
                                    (floatInterp playProgress (0.7 * width) (0.7 * 0.13 * radius))
                                    (floatInterp playProgress height (0.13 * radius))
                                    1
                                )
                                (vec3 0.52 0.1 0.2)
                        , Primitives.quad Clock.Shaders.fragment <|
                            locals texture
                                pos
                                (makeScale3
                                    (floatInterp playProgress width (0.13 * radius))
                                    (floatInterp playProgress height (0.13 * radius))
                                    1
                                )
                                rot
                                (vec3 1 1 1)
                        ]

                otherwise ->
                    []
    in
        mainView ++ extraView
