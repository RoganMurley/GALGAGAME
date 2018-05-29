module Clock.Hand exposing (..)

import Animation.Types exposing (Anim(..))
import Animation.State exposing (animToResTickMax)
import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (uniforms)
import Clock.Types exposing (ClockParams)
import Ease
import Hand.Types exposing (Hand)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (floatInterp, interp)


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


handView : ClockParams -> Hand -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
handView ({ w, h, radius } as params) finalHand resInfo texture =
    let
        locals =
            uniforms 0 ( floor w, floor h )

        ( hand, drawingCard ) =
            case resInfo of
                Just ( _, Just (Draw PlayerA) ) ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                otherwise ->
                    ( finalHand, Nothing )

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

        entity : Int -> WebGL.Entity
        entity i =
            Primitives.quad Clock.Shaders.fragment <|
                locals texture
                    (interp progress (position params PlayerA i n) (position params PlayerA i finalN))
                    (makeScale3 width height 1)
                    (makeRotate (floatInterp progress (rotation PlayerA i n) (rotation PlayerA i finalN)) <| vec3 0 0 1)
                    (vec3 1 1 1)

        mainView : List WebGL.Entity
        mainView =
            List.map entity (List.range 0 (n - 1))

        drawView : List WebGL.Entity
        drawView =
            case drawingCard of
                Nothing ->
                    []

                Just _ ->
                    [ Primitives.quad Clock.Shaders.fragment <|
                        locals texture
                            (interp
                                progress
                                (vec3 w h 0)
                                (position params PlayerA n (n + 1))
                            )
                            (makeScale3 width height 1)
                            (makeRotate (floatInterp progress 0 (rotation PlayerA n (n + 1))) <|
                                vec3 0 0 1
                            )
                            (vec3 1 1 1)
                    ]
    in
        mainView ++ drawView


otherHandView : ClockParams -> Int -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
otherHandView ({ w, h, radius } as params) finalN resInfo texture =
    let
        locals =
            uniforms 0 ( floor w, floor h )

        ( n, drawingCard ) =
            case resInfo of
                Just ( _, Just (Draw PlayerB) ) ->
                    ( finalN - 1, True )

                otherwise ->
                    ( finalN, False )

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

        entity : Int -> WebGL.Entity
        entity i =
            Primitives.quad Clock.Shaders.fragment <|
                locals texture
                    (interp progress (position params PlayerB i n) (position params PlayerB i finalN))
                    (makeScale3 width height 1)
                    (makeRotate
                        (floatInterp progress (rotation PlayerB i n) (rotation PlayerB i finalN))
                        (vec3 0 0 1)
                    )
                    (vec3 1 1 1)

        mainView : List WebGL.Entity
        mainView =
            List.map entity (List.range 0 (n - 1))

        drawView : List WebGL.Entity
        drawView =
            case drawingCard of
                False ->
                    []

                True ->
                    [ Primitives.quad Clock.Shaders.fragment <|
                        locals texture
                            (interp
                                progress
                                (vec3 w 0 0)
                                (position params PlayerB n (n + 1))
                            )
                            (makeScale3 width height 1)
                            (makeRotate
                                (floatInterp progress (0.5 * pi) (rotation PlayerB n (n + 1)))
                                (vec3 0 0 1)
                            )
                            (vec3 1 1 1)
                    ]
    in
        mainView ++ drawView
