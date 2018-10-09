module Hand.Entities exposing (..)

import Animation.Types exposing (Anim(..))
import Card.Types as Card exposing (Card)
import Game.Types exposing (Context)
import Math.Vector2 exposing (Vec2, vec2)
import Model.Entity exposing (GameEntity)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (interpFloat, interp2D)


entities : Context -> List (Card.Entity { index : Int })
entities ({ w, h, radius, anim, model, progress } as ctx) =
    let
        finalHand =
            model.hand

        ( hand, drawingCard ) =
            case anim of
                Draw PlayerA ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
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
                        (handCardPosition ctx PlayerA i n)
                        (handCardPosition ctx PlayerA finalI finalN)

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
            List.map entity <| List.indexedMap (,) hand

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
                                        (handCardPosition ctx PlayerA n (n + 1))

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
                                (handCardPosition ctx PlayerA i n)
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

                _ ->
                    []
    in
        mainEntities ++ extraEntities


otherEntities : Context -> List (GameEntity {})
otherEntities ({ w, h, radius, anim, model, progress } as ctx) =
    let
        finalN =
            model.otherHand

        n =
            case anim of
                Draw PlayerB ->
                    finalN - 1

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

        entity : Int -> GameEntity {}
        entity finalI =
            let
                i =
                    indexModifier finalI
            in
                { position =
                    interp2D progress
                        (handCardPosition ctx PlayerB i n)
                        (handCardPosition ctx PlayerB finalI finalN)
                , rotation =
                    interpFloat progress
                        (handCardRotation PlayerB i n)
                        (handCardRotation PlayerB finalI finalN)
                , scale = 1
                }

        mainEntities : List (GameEntity {})
        mainEntities =
            List.map entity (List.range 0 (n - 1))

        extraEntities : List (GameEntity {})
        extraEntities =
            case anim of
                Draw PlayerB ->
                    [ { position =
                            interp2D progress
                                (vec2 w 0)
                                (handCardPosition ctx PlayerB n (n + 1))
                      , rotation =
                            interpFloat progress 0 (handCardRotation PlayerB n (n + 1))
                      , scale = 1
                      }
                    ]

                Play PlayerB _ i ->
                    [ { position =
                            interp2D progress
                                (handCardPosition ctx PlayerB i n)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))
                      , rotation =
                            interpFloat progress (handCardRotation PlayerB i n) 0
                      , scale =
                            interpFloat progress 1 1.3
                      }
                    ]

                _ ->
                    []
    in
        mainEntities ++ extraEntities


handOrigin : Context -> WhichPlayer -> Int -> Vec2
handOrigin { w, h, radius } which count =
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


handCardPosition : Context -> WhichPlayer -> Int -> Int -> Vec2
handCardPosition ({ radius } as ctx) which index count =
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
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                sign * (abs <| 4 * (toFloat <| ceiling (i - (c * 0.5))))
    in
        Math.Vector2.add
            (handOrigin ctx which count)
        <|
            Math.Vector2.add
                (vec2 (toFloat index * (width + spacing)) 0)
            <|
                vec2 0 y
