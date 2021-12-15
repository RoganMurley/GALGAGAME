module Hand.Entities exposing (entities, handCardPosition, handCardRotation, handOrigin, otherEntities, playPosition)

import Animation.State as Animation
import Animation.Types exposing (Anim(..), Bounce, CardDiscard(..), HandBounce, KnowableCard(..))
import Array
import Card.State as Card
import Card.Types exposing (Card)
import Game.Entity as Game
import Game.Types exposing (Context, HandEntity, OtherHandEntity)
import Holding.State as Holding
import Holding.Types exposing (Holding(..))
import Hover exposing (Hover(..), HoverOther, HoverSelf)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Quaternion
import Stack.Entities
import Util exposing (interp, interpFloat)
import WhichPlayer.Types exposing (WhichPlayer(..))


width : Float
width =
    0.0001


spacing : Float
spacing =
    0.125


entities : HoverSelf -> Holding -> Context -> List HandEntity
entities hover holding ({ anim, model, progress } as ctx) =
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
                Draw PlayerA _ ->
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
                Play PlayerA _ index _ ->
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

        holdingIndex =
            Holding.getHandIndex holding

        entity : ( Int, Card ) -> Maybe HandEntity
        entity ( finalI, card ) =
            let
                i =
                    indexModifier finalI

                pos =
                    interp progress
                        (handCardPosition ctx PlayerA i n hover)
                        (handCardPosition ctx PlayerA finalI finalN hover)

                rot =
                    Quaternion.lerp progress
                        (Quaternion.zRotation (handCardRotation PlayerA i n))
                        (Quaternion.zRotation (handCardRotation PlayerA finalI finalN))
            in
            if Just finalI == holdingIndex then
                Nothing

            else
                Just <|
                    { position = pos
                    , rotation = rot
                    , scale = Card.scale
                    , card = card
                    , owner = PlayerA
                    , index = finalI
                    }

        mainEntities : List HandEntity
        mainEntities =
            Maybe.values <| List.map entity <| List.indexedMap (\a b -> ( a, b )) hand

        extraEntities : List HandEntity
        extraEntities =
            case anim of
                Draw PlayerA _ ->
                    case drawingCard of
                        Just card ->
                            let
                                pos =
                                    interp progress
                                        (vec3 -1 -1 -1)
                                        (handCardPosition ctx PlayerA n (n + 1) hover)

                                rot =
                                    Quaternion.lerp
                                        progress
                                        Quaternion.identity
                                        (Quaternion.zRotation (handCardRotation PlayerA n (n + 1)))
                            in
                            [ { position = pos
                              , rotation = rot
                              , scale = Card.scale
                              , card = card
                              , owner = PlayerA
                              , index = n
                              }
                            ]

                        Nothing ->
                            []

                Play PlayerA knowableCard i mStartPos ->
                    let
                        startPos =
                            Maybe.withDefault
                                (handCardPosition ctx PlayerA i n hover)
                                mStartPos

                        pos =
                            interp progress
                                startPos
                                (playPosition ctx)

                        rot =
                            Quaternion.lerp
                                progress
                                (Quaternion.zRotation (handCardRotation PlayerA n (n + 1)))
                                (Quaternion.xRotation (-0.35 * pi))

                        scale =
                            Card.scale

                        card =
                            case knowableCard of
                                KnownCard c ->
                                    c

                                UnknownCard c ->
                                    c
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
                                    Stack.Entities.stackEntity ctx stackIndex
                            in
                            { owner = PlayerA
                            , card = card
                            , index = handIndex
                            , position =
                                interp progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerA handIndex finalN hover)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.zRotation (handCardRotation PlayerA handIndex finalN))
                            , scale = Card.scale
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


otherEntities : HoverOther -> Context -> List OtherHandEntity
otherEntities hover ({ anim, model, progress } as ctx) =
    let
        -- DRY with PlayerA entities
        finalHand =
            case anim of
                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack
                    in
                    model.otherHand ++ List.map (Just << .card) playerBounces

                _ ->
                    model.otherHand

        ( hand, drawingCard ) =
            case anim of
                Draw PlayerB _ ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , Maybe.join <| List.head <| List.reverse finalHand
                    )

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack
                    in
                    ( List.take (List.length finalHand - List.length playerBounces) finalHand
                    , Nothing
                    )

                _ ->
                    ( finalHand, Nothing )

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Play PlayerB _ index _ ->
                    \i ->
                        if i >= index then
                            i + 1

                        else
                            i

                _ ->
                    identity

        n =
            List.length hand

        finalN =
            List.length finalHand

        entity : ( Int, Maybe Card ) -> OtherHandEntity
        entity ( finalI, mCard ) =
            let
                i =
                    indexModifier finalI
            in
            { position =
                interp progress
                    (handCardPosition ctx PlayerB i n hover)
                    (handCardPosition ctx PlayerB finalI finalN hover)
            , rotation =
                Quaternion.lerp
                    progress
                    (Quaternion.zRotation (handCardRotation PlayerB i n))
                    (Quaternion.zRotation (handCardRotation PlayerB finalI finalN))
            , scale = Card.scale
            , mCard = mCard
            }

        mainEntities : List OtherHandEntity
        mainEntities =
            List.map entity <| List.indexedMap (\a b -> ( a, b )) hand

        extraEntities : List OtherHandEntity
        extraEntities =
            case anim of
                Draw PlayerB _ ->
                    [ { position =
                            interp progress
                                (vec3 -1 1 -1)
                                (handCardPosition ctx PlayerB n (n + 1) hover)
                      , rotation =
                            Quaternion.lerp
                                progress
                                Quaternion.identity
                                (Quaternion.zRotation (handCardRotation PlayerB n (n + 1)))
                      , scale = Card.scale
                      , mCard = drawingCard
                      }
                    ]

                Play PlayerB knowableCard i _ ->
                    [ { position =
                            interp progress
                                (handCardPosition ctx PlayerB i n hover)
                                (playPosition ctx)
                      , rotation =
                            Quaternion.lerp
                                progress
                                (Quaternion.zRotation (handCardRotation PlayerB i n))
                                (Quaternion.xRotation (-0.35 * pi))
                      , scale = Card.scale
                      , mCard =
                            case knowableCard of
                                KnownCard card ->
                                    Just card

                                UnknownCard _ ->
                                    Nothing
                      }
                    ]

                Bounce bounces ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerB bounces model.stack

                        makeBounceEntity : HandBounce -> OtherHandEntity
                        makeBounceEntity { stackIndex, handIndex, card } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx stackIndex
                            in
                            { position =
                                interp progress
                                    stackEntity.position
                                    (handCardPosition ctx PlayerB handIndex finalN hover)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.zRotation (handCardRotation PlayerB handIndex finalN))
                            , scale = Card.scale
                            , mCard = Just card
                            }
                    in
                    List.map makeBounceEntity playerBounces

                _ ->
                    []
    in
    mainEntities ++ extraEntities


handOrigin : Context -> WhichPlayer -> Int -> Vec3
handOrigin { anim, tick, w, h } which count =
    let
        shake =
            0.01 * Animation.animShake anim which tick

        x =
            0.5 * (width + spacing) * (toFloat <| count - 1)

        y =
            case which of
                PlayerA ->
                    -0.64

                PlayerB ->
                    0.7

        baseZ =
            -0.35 + shake

        z =
            if w >= h then
                0

            else if w / h < 0.5 then
                0.15

            else if w / h < 0.55 then
                0.1

            else if w / h < 0.6 then
                0.05

            else
                0.025
    in
    vec3 x y (baseZ + z)


handCardRotation : WhichPlayer -> Int -> Int -> Float
handCardRotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
    case which of
        PlayerA ->
            magnitude

        PlayerB ->
            -magnitude


handCardPosition : Context -> WhichPlayer -> Int -> Int -> Hover a -> Vec3
handCardPosition ctx which index count hover =
    let
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

                discardY =
                    case ctx.anim of
                        DiscardHand w discards ->
                            if w /= which then
                                0

                            else
                                case Array.get index <| Array.fromList discards of
                                    Just CardDiscard ->
                                        -10 * ctx.progress

                                    _ ->
                                        0

                        _ ->
                            0

                baseY =
                    abs <| 4 * (toFloat <| ceiling (i - (c * 0.5)))
            in
            sign * (baseY + hoverY + discardY)
    in
    vec3 -x (-0.004 * y) 0
        |> Math.Vector3.add
            (handOrigin ctx which count)


playPosition : Context -> Vec3
playPosition ctx =
    let
        entity =
            Stack.Entities.wheelEntity ctx (Stack.Entities.baseDistance ctx) 0 0
    in
    entity.position
