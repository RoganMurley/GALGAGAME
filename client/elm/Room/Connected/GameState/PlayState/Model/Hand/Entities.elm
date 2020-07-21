module Hand.Entities exposing (entities, handCardPosition, handCardRotation, handOrigin, otherEntities, playPosition)

import Animation.State as Animation
import Animation.Types exposing (Anim(..), Bounce, HandBounce)
import Card.Types exposing (Card)
import Game.Entity as Game
import Game.Types exposing (Context, HandEntity, OtherHandEntity)
import Hover exposing (Hover(..), HoverOther, HoverSelf)
import Math.Matrix4 exposing (Mat4, makeRotate)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Stack.Entities
import Util exposing (interp, interp2D, interpFloat)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : HoverSelf -> Context -> List HandEntity
entities hover ({ w, h, anim, model, progress } as ctx) =
    let
        finalHand =
            case anim of
                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerA bounces model.stack
                    in
                    model.hand ++ List.map .card playerBounces

                _ ->
                    model.hand

        ( hand, drawingCard ) =
            case anim of
                Draw PlayerA ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerA bounces model.stack
                    in
                    ( List.take (List.length finalHand - List.length playerBounces) finalHand
                    , Nothing
                    )

                _ ->
                    ( finalHand, Nothing )

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Play PlayerA _ index ->
                    \i ->
                        if i >= index then
                            i + 1

                        else
                            i

                _ ->
                    Basics.identity

        n =
            List.length hand

        finalN =
            List.length finalHand

        entity : ( Int, Card ) -> HandEntity
        entity ( finalI, card ) =
            let
                i =
                    indexModifier finalI

                pos =
                    interp progress
                        (handCardPosition ctx PlayerA i n hover)
                        (handCardPosition ctx PlayerA finalI finalN hover)

                rot =
                    makeRotate
                        (interpFloat progress
                            (handCardRotation PlayerA i n)
                            (handCardRotation PlayerA finalI finalN)
                        )
                        (vec3 0 0 1)
            in
            { position = pos
            , rotation = rot
            , scale = 0.003
            , card = card
            , owner = PlayerA
            , index = finalI
            }

        mainEntities : List HandEntity
        mainEntities =
            List.map entity <| List.indexedMap (\a b -> ( a, b )) hand

        extraEntities : List HandEntity
        extraEntities =
            case anim of
                Draw PlayerA ->
                    case drawingCard of
                        Just card ->
                            let
                                pos =
                                    interp progress
                                        (vec3 1 -1 0)
                                        (handCardPosition ctx PlayerA n (n + 1) hover)

                                rot =
                                    makeRotate
                                        (interpFloat progress 0 (handCardRotation PlayerA n (n + 1)))
                                        (vec3 0 0 1)
                            in
                            [ { position = pos
                              , rotation = rot
                              , scale = 0.003
                              , card = card
                              , owner = PlayerA
                              , index = n
                              }
                            ]

                        Nothing ->
                            []

                Play PlayerA card i ->
                    let
                        pos =
                            interp progress
                                (handCardPosition ctx PlayerA i n hover)
                                (playPosition ctx)

                        rot =
                            makeRotate (interpFloat progress (handCardRotation PlayerA i n) pi) (vec3 0 0 1)

                        scale =
                            0.003
                    in
                    [ { position = pos
                      , rotation = rot
                      , scale = scale
                      , card = card
                      , owner = PlayerA
                      , index = i
                      }
                    ]

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerA bounces model.stack

                        makeBounceEntity : HandBounce -> HandEntity
                        makeBounceEntity { handIndex, stackIndex, card } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx 0 (List.length model.stack) stackIndex
                            in
                            { owner = PlayerA
                            , card = card
                            , index = handIndex
                            , position =
                                interp progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerA handIndex finalN hover)
                            , rotation = makeRotate 0 (vec3 0 0 1)

                            --
                            -- interpFloat progress
                            --     stackEntity.rotation
                            --     (handCardRotation PlayerA handIndex finalN)
                            , scale =
                                0.003
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


otherEntities : HoverOther -> Context -> List (Game.Entity3D {})
otherEntities hover ({ w, anim, model, progress } as ctx) =
    let
        finalN =
            case anim of
                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack
                    in
                    model.otherHand + List.length playerBounces

                _ ->
                    model.otherHand

        n =
            case anim of
                Draw PlayerB ->
                    finalN - 1

                Bounce _ ->
                    model.otherHand

                _ ->
                    finalN

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Play PlayerB _ index ->
                    \i ->
                        if i >= index then
                            i + 1

                        else
                            i

                _ ->
                    identity

        entity : Int -> OtherHandEntity
        entity finalI =
            let
                i =
                    indexModifier finalI
            in
            { position =
                interp progress
                    (handCardPosition ctx PlayerB i n hover)
                    (handCardPosition ctx PlayerB finalI finalN hover)
            , rotation =
                makeRotate
                    (interpFloat progress
                        (handCardRotation PlayerB i n)
                        (handCardRotation PlayerB finalI finalN)
                    )
                    (vec3 0 0 1)
            , scale = 0.003
            }

        mainEntities : List OtherHandEntity
        mainEntities =
            List.map entity (List.range 0 (n - 1))

        extraEntities : List OtherHandEntity
        extraEntities =
            case anim of
                Draw PlayerB ->
                    [ { position =
                            interp progress
                                (vec3 1 1 0)
                                (handCardPosition ctx PlayerB n (n + 1) hover)
                      , rotation =
                            makeRotate
                                (interpFloat progress 0 (handCardRotation PlayerB n (n + 1)))
                                (vec3 0 0 1)
                      , scale = 0.003
                      }
                    ]

                Play PlayerB _ i ->
                    [ { position =
                            interp progress
                                (handCardPosition ctx PlayerB i n hover)
                                (playPosition ctx)
                      , rotation =
                            makeRotate
                                (interpFloat progress (handCardRotation PlayerB i n) 0)
                                (vec3 0 0 1)
                      , scale =
                            0.003
                      }
                    ]

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack

                        makeBounceEntity : HandBounce -> Game.Entity3D {}
                        makeBounceEntity { stackIndex, handIndex } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx 0 (List.length model.stack) stackIndex
                            in
                            { position =
                                interp progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerB handIndex finalN hover)
                            , rotation = makeRotate 0 (vec3 0 0 1)

                            -- , rotation =
                            --     makeRotate
                            --         (interpFloat progress
                            --             stackEntity.rotation
                            --             (handCardRotation PlayerB handIndex finalN)
                            --         )
                            --         (vec3 0 0 1)
                            , scale =
                                0.003
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


handOrigin : Context -> WhichPlayer -> Int -> Vec3
handOrigin { w, h, radius, anim, tick } which count =
    let
        ( width, height, spacing ) =
            ( 0.0001, 0.0001, 0.2 )

        shake =
            0.001 * Animation.animShake anim which tick

        x =
            -0.5 * (width + spacing) * (toFloat <| count - 1)

        y =
            case which of
                PlayerA ->
                    -0.6

                PlayerB ->
                    0.6
    in
    vec3 x y 0.5


handCardRotation : WhichPlayer -> Int -> Int -> Float
handCardRotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
    case which of
        PlayerA ->
            -magnitude

        PlayerB ->
            magnitude


handCardPosition : Context -> WhichPlayer -> Int -> Int -> Hover a -> Vec3
handCardPosition ({ radius } as ctx) which index count hover =
    let
        ( width, _, spacing ) =
            ( 0.0001, 0.0001, 0.2 )

        sign =
            case which of
                PlayerA ->
                    1

                PlayerB ->
                    -1

        x =
            toFloat index * (width + spacing)

        y =
            let
                i =
                    if modBy 2 count == 0 && index < count // 2 then
                        toFloat <| index + 1

                    else
                        toFloat index

                c =
                    toFloat count

                hoverY =
                    case hover of
                        HoverHand hoverHand ->
                            if index == hoverHand.index then
                                interpFloat (hoverHand.tick / 70) 0 -10

                            else
                                0

                        _ ->
                            0

                baseY =
                    abs <| 4 * (toFloat <| ceiling (i - (c * 0.5)))
            in
            sign * (baseY + hoverY)
    in
    vec3 x (-0.004 * y) 0
        |> Math.Vector3.add
            (handOrigin ctx which count)


playPosition : Context -> Vec3
playPosition { w, h, radius } =
    vec3 0 0.615 0
