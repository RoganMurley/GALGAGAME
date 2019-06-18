module Hand.Entities exposing (entities, handCardPosition, handCardRotation, handOrigin, otherEntities)

import Animation.State as Animation
import Animation.Types exposing (Anim(..), Bounce, HandBounce)
import Card.Types as Card exposing (Card)
import Game.Entity as Game
import Game.Types exposing (Context, Hover(..))
import Math.Vector2 exposing (Vec2, vec2)
import Stack.Entities
import Util exposing (interp2D, interpFloat)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : Hover { dmg : ( Int, Int ) } -> Context -> List (Card.Entity { index : Int })
entities hover ({ w, h, radius, anim, model, progress } as ctx) =
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

        entity : ( Int, Card ) -> Card.Entity { index : Int }
        entity ( finalI, card ) =
            let
                i =
                    indexModifier finalI

                pos =
                    interp2D progress
                        (handCardPosition ctx PlayerA i n hover)
                        (handCardPosition ctx PlayerA finalI finalN hover)

                rot =
                    interpFloat progress
                        (handCardRotation PlayerA i n)
                        (handCardRotation PlayerA finalI finalN)
            in
            { position = pos
            , rotation = rot
            , scale = 1
            , card = card
            , owner = PlayerA
            , index = finalI
            }

        mainEntities : List (Card.Entity { index : Int })
        mainEntities =
            List.map entity <| List.indexedMap (\a b -> ( a, b )) hand

        extraEntities : List (Card.Entity { index : Int })
        extraEntities =
            case anim of
                Draw PlayerA ->
                    case drawingCard of
                        Just card ->
                            let
                                pos =
                                    interp2D progress
                                        (vec2 w h)
                                        (handCardPosition ctx PlayerA n (n + 1) hover)

                                rot =
                                    interpFloat progress 0 (handCardRotation PlayerA n (n + 1))
                            in
                            [ { position = pos
                              , rotation = rot
                              , scale = 1
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
                            interp2D progress
                                (handCardPosition ctx PlayerA i n hover)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))

                        rot =
                            interpFloat progress (handCardRotation PlayerA i n) pi

                        scale =
                            interpFloat progress 1 1.3
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

                        makeBounceEntity : HandBounce -> Card.Entity { index : Int }
                        makeBounceEntity { handIndex, stackIndex, card } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx 0 (List.length model.stack) stackIndex
                            in
                            { owner = PlayerA
                            , card = card
                            , index = handIndex
                            , position =
                                interp2D progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerA handIndex finalN hover)
                            , rotation =
                                interpFloat progress
                                    stackEntity.rotation
                                    (handCardRotation PlayerA handIndex finalN)
                            , scale =
                                interpFloat progress stackEntity.scale 1
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


otherEntities : Hover {} -> Context -> List (Game.Entity {})
otherEntities hover ({ w, h, radius, anim, model, progress } as ctx) =
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

        entity : Int -> Game.Entity {}
        entity finalI =
            let
                i =
                    indexModifier finalI
            in
            { position =
                interp2D progress
                    (handCardPosition ctx PlayerB i n hover)
                    (handCardPosition ctx PlayerB finalI finalN hover)
            , rotation =
                interpFloat progress
                    (handCardRotation PlayerB i n)
                    (handCardRotation PlayerB finalI finalN)
            , scale = 1
            }

        mainEntities : List (Game.Entity {})
        mainEntities =
            List.map entity (List.range 0 (n - 1))

        extraEntities : List (Game.Entity {})
        extraEntities =
            case anim of
                Draw PlayerB ->
                    [ { position =
                            interp2D progress
                                (vec2 w 0)
                                (handCardPosition ctx PlayerB n (n + 1) hover)
                      , rotation =
                            interpFloat progress 0 (handCardRotation PlayerB n (n + 1))
                      , scale = 1
                      }
                    ]

                Play PlayerB _ i ->
                    [ { position =
                            interp2D progress
                                (handCardPosition ctx PlayerB i n hover)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))
                      , rotation =
                            interpFloat progress (handCardRotation PlayerB i n) 0
                      , scale =
                            interpFloat progress 1 1.3
                      }
                    ]

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack

                        makeBounceEntity : HandBounce -> Game.Entity {}
                        makeBounceEntity { stackIndex, handIndex } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx 0 (List.length model.stack) stackIndex
                            in
                            { position =
                                interp2D progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerB handIndex finalN hover)
                            , rotation =
                                interpFloat progress
                                    stackEntity.rotation
                                    (handCardRotation PlayerB handIndex finalN)
                            , scale =
                                interpFloat progress stackEntity.scale 1
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


handOrigin : Context -> WhichPlayer -> Int -> Vec2
handOrigin { w, h, radius, anim, tick } which count =
    let
        ( width, height, spacing ) =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        shake =
            Animation.animShake anim which tick

        x =
            w / 2 - 0.5 * (width + spacing) * (toFloat <| count - 1) + shake

        y =
            case which of
                PlayerA ->
                    h - height - shake

                PlayerB ->
                    height + shake
    in
    vec2 x y


handCardRotation : WhichPlayer -> Int -> Int -> Float
handCardRotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
    case which of
        PlayerA ->
            pi + magnitude

        PlayerB ->
            -magnitude


handCardPosition : Context -> WhichPlayer -> Int -> Int -> Hover a -> Vec2
handCardPosition ({ radius } as ctx) which index count hover =
    let
        ( width, _, spacing ) =
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
    Math.Vector2.add
        (handOrigin ctx which count)
    <|
        Math.Vector2.add
            (vec2 (toFloat index * (width + spacing)) 0)
        <|
            vec2 0 y
