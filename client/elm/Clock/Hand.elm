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


handView : ClockParams -> Hand -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
handView { w, h, radius } finalHand resInfo texture =
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
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        origin : Int -> Vec3
        origin count =
            vec3
                ((w / 2) - (0.5 * (width + spacing) * (toFloat (count - 1))))
                (h - height)
                0

        entity : Int -> WebGL.Entity
        entity i =
            Primitives.quad Clock.Shaders.fragment <|
                locals texture
                    (interp progress (getPos i n) (getPos i finalN))
                    (makeScale3 width height 1)
                    (makeRotate (floatInterp progress (getRot i n) (getRot i finalN)) <| vec3 0 0 1)
                    (vec3 1 1 1)

        getPos : Int -> Int -> Vec3
        getPos i count =
            Math.Vector3.add (origin count) <|
                Math.Vector3.add (vec3 ((toFloat i) * (width + spacing)) 0 0) <|
                    vec3 0 (getY i count) 0

        getY : Int -> Int -> Float
        getY index count =
            let
                i =
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                abs <| 4 * (toFloat <| ceiling (i - (c * 0.5)))

        getRot : Int -> Int -> Float
        getRot i count =
            pi + 0.05 * (toFloat <| ceiling <| toFloat i - ((toFloat count) * 0.5))

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
                                (getPos n (n + 1))
                            )
                            (makeScale3 width height 1)
                            (makeRotate (floatInterp progress 0 (getRot n (n + 1))) <|
                                vec3 0 0 1
                            )
                            (vec3 1 1 1)
                    ]
    in
        mainView ++ drawView


otherHandView : ClockParams -> Int -> Maybe ( Float, Maybe Anim ) -> Texture -> List WebGL.Entity
otherHandView { w, h, radius } finalN resInfo texture =
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
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        getPos : Int -> Int -> Vec3
        getPos i count =
            Math.Vector3.add
                (origin count)
            <|
                Math.Vector3.add
                    (vec3 ((toFloat i) * (width + spacing)) 0 0)
                <|
                    vec3 0 (getY i count) 0

        getY : Int -> Int -> Float
        getY index count =
            let
                i =
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                -(abs <| 4 * (toFloat <| ceiling (i - (c * 0.5))))

        getRot : Int -> Int -> Float
        getRot i count =
            -0.05 * (toFloat <| ceiling <| toFloat i - ((toFloat count) * 0.5))

        origin count =
            vec3
                ((w / 2) - (0.5 * (width + spacing) * (toFloat (count - 1))))
                height
                0

        entity : Int -> WebGL.Entity
        entity i =
            Primitives.quad Clock.Shaders.fragment <|
                locals texture
                    (interp progress (getPos i n) (getPos i finalN))
                    (makeScale3 width height 1)
                    (makeRotate
                        (floatInterp progress (getRot i n) (getRot i finalN))
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
                                (getPos n (n + 1))
                            )
                            (makeScale3 width height 1)
                            (makeRotate
                                (floatInterp progress (0.5 * pi) (getRot n (n + 1)))
                                (vec3 0 0 1)
                            )
                            (vec3 1 1 1)
                    ]
    in
        mainView ++ drawView
