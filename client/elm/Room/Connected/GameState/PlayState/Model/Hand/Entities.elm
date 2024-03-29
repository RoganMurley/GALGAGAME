module Hand.Entities exposing (applyHoverVector, entities, handCardPosition, handCardRotation, handOrigin, otherEntities, playPosition)

import Animation.State as Animation
import Animation.Types exposing (Anim(..), CardDiscard(..), DeckBounce, HandBounce)
import Card.State as Card exposing (getCard, isRevealed)
import Card.Types exposing (Card, KnowableCard(..))
import Dict
import Game.Types exposing (Context, HandEntity, OtherHandEntity)
import Holding.State as Holding
import Holding.Types exposing (Holding(..))
import Hover exposing (Hover(..), HoverDamage(..), HoverOther, HoverSelf)
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
                Bounce bounces _ ->
                    let
                        playerBounces : List HandBounce
                        playerBounces =
                            Animation.getPlayerBounceCards PlayerA bounces model.stack
                    in
                    model.hand ++ List.map (KnownCard << .card) playerBounces

                _ ->
                    model.hand

        ( hand, drawingCard ) =
            case anim of
                Draw PlayerA _ ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                Bounce bounces _ ->
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

                DiscardHand PlayerA discards ->
                    let
                        posDict =
                            Dict.fromList <|
                                List.filterMap
                                    (\( i, discard ) ->
                                        case discard of
                                            NoDiscard _ targetI ->
                                                Just ( targetI, i )

                                            CardDiscard _ ->
                                                Nothing
                                    )
                                <|
                                    List.indexedMap Tuple.pair
                                        discards
                    in
                    \i ->
                        case Dict.get i posDict of
                            Just targetI ->
                                targetI

                            Nothing ->
                                i

                _ ->
                    Basics.identity

        n =
            List.length hand

        finalN =
            List.length finalHand

        holdingIndex =
            Holding.getHandIndex holding

        entity : ( Int, KnowableCard ) -> Maybe HandEntity
        entity ( finalI, knowableCard ) =
            let
                card =
                    getCard knowableCard

                revealed =
                    isRevealed knowableCard

                i =
                    indexModifier finalI

                initialPos =
                    handCardPosition ctx PlayerA i n hover

                finalPos =
                    handCardPosition ctx PlayerA finalI finalN hover

                pos =
                    interp progress
                        initialPos.position
                        finalPos.position

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
                    , revealed = revealed
                    , discarding = False
                    , hoverVector = initialPos.hoverVector
                    }

        mainEntities : List HandEntity
        mainEntities =
            Maybe.values <|
                List.map entity <|
                    List.indexedMap
                        (\a b -> ( a, b ))
                        hand

        extraEntities : List HandEntity
        extraEntities =
            case anim of
                Draw PlayerA _ ->
                    case drawingCard of
                        Just knowableCard ->
                            let
                                card =
                                    getCard knowableCard

                                pos =
                                    interp progress
                                        (vec3 -1 -1 -1)
                                        (.position <| handCardPosition ctx PlayerA n (n + 1) hover)

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
                              , revealed = False
                              , discarding = False
                              , hoverVector = vec3 0 0 0
                              }
                            ]

                        Nothing ->
                            []

                Play PlayerA knowableCard i mStartPos ->
                    let
                        startPos =
                            Maybe.withDefault
                                (.position <| handCardPosition ctx PlayerA i n hover)
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
                            getCard knowableCard

                        revealed =
                            isRevealed knowableCard
                    in
                    [ { position = pos
                      , rotation = rot
                      , scale = scale
                      , card = card
                      , owner = PlayerA
                      , index = i
                      , revealed = revealed
                      , discarding = False
                      , hoverVector = vec3 0 0 0
                      }
                    ]

                Bounce bounces _ ->
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
                                    (.position <| handCardPosition ctx PlayerA handIndex finalN hover)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.zRotation (handCardRotation PlayerA handIndex finalN))
                            , scale = Card.scale
                            , revealed = True
                            , discarding = False
                            , hoverVector = vec3 0 0 0
                            }
                    in
                    List.map makeBounceEntity playerBounces

                BounceDeck bounces _ ->
                    let
                        playerBounces : List DeckBounce
                        playerBounces =
                            Animation.getPlayerBounceDeckCards PlayerA bounces model.stack

                        makeBounceEntity : DeckBounce -> HandEntity
                        makeBounceEntity { stackIndex, card } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx stackIndex
                            in
                            { owner = PlayerA
                            , card = card
                            , index = 0 -- ignore
                            , position =
                                interp progress
                                    stackEntity.position
                                    (vec3 -1 -(1 + 0.1 * toFloat stackIndex) -1)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.xRotation (-0.35 * pi))
                            , scale = Card.scale
                            , revealed = True
                            , discarding = False
                            , hoverVector = vec3 0 0 0
                            }
                    in
                    List.map makeBounceEntity playerBounces

                DiscardHand PlayerA discards ->
                    let
                        cardDiscards =
                            List.filterMap
                                (\( index, discard ) ->
                                    case discard of
                                        NoDiscard _ _ ->
                                            Nothing

                                        CardDiscard card ->
                                            Just ( index, card )
                                )
                            <|
                                List.indexedMap Tuple.pair
                                    discards

                        makeDiscardEntity : ( Int, KnowableCard ) -> HandEntity
                        makeDiscardEntity ( index, card ) =
                            let
                                initialPosition =
                                    .position <| handCardPosition ctx PlayerA index (n + List.length cardDiscards) hover

                                finalPosition =
                                    Math.Vector3.add initialPosition (vec3 0 0.1 0.05)
                            in
                            { owner = PlayerA
                            , card = getCard card
                            , index = index
                            , position = interp progress initialPosition finalPosition
                            , rotation = Quaternion.zRotation (handCardRotation PlayerA index (n + List.length cardDiscards))
                            , scale = Card.scale
                            , revealed = True
                            , discarding = True
                            , hoverVector = vec3 0 0 0
                            }
                    in
                    List.map makeDiscardEntity cardDiscards

                _ ->
                    []
    in
    mainEntities ++ extraEntities


otherEntities : HoverSelf -> HoverOther -> Context -> List OtherHandEntity
otherEntities hoverSelf hoverOther ({ anim, model, progress } as ctx) =
    let
        hover : Hover {}
        hover =
            case ( hoverSelf, hoverOther ) of
                ( HoverOtherHand { index, tick }, _ ) ->
                    HoverOtherHand { index = index, tick = tick }

                ( _, HoverHand { index, tick } ) ->
                    HoverOtherHand { index = index, tick = tick }

                _ ->
                    NoHover

        -- DRY with PlayerA entities
        finalHand =
            case anim of
                Bounce bounces _ ->
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

                Bounce bounces _ ->
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

                DiscardHand PlayerB discards ->
                    let
                        posDict =
                            Dict.fromList <|
                                List.filterMap
                                    (\( i, discard ) ->
                                        case discard of
                                            NoDiscard _ targetI ->
                                                Just ( targetI, i )

                                            CardDiscard _ ->
                                                Nothing
                                    )
                                <|
                                    List.indexedMap Tuple.pair
                                        discards
                    in
                    \i ->
                        case Dict.get i posDict of
                            Just targetI ->
                                targetI

                            Nothing ->
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
                    (.position <| applyHoverVector <| handCardPosition ctx PlayerB i n hover)
                    (.position <| applyHoverVector <| handCardPosition ctx PlayerB finalI finalN hover)
            , rotation =
                Quaternion.lerp
                    progress
                    (Quaternion.zRotation (handCardRotation PlayerB i n))
                    (Quaternion.zRotation (handCardRotation PlayerB finalI finalN))
            , scale = Card.scale
            , mCard = mCard
            , discarding = False
            , index = finalI
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
                                (.position <| handCardPosition ctx PlayerB n (n + 1) hover)
                      , rotation =
                            Quaternion.lerp
                                progress
                                Quaternion.identity
                                (Quaternion.zRotation (handCardRotation PlayerB n (n + 1)))
                      , scale = Card.scale
                      , mCard = drawingCard
                      , discarding = False
                      , index = n
                      }
                    ]

                Play PlayerB knowableCard i _ ->
                    [ { position =
                            interp progress
                                (.position <| handCardPosition ctx PlayerB i n hover)
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
                      , index = i
                      , discarding = False
                      }
                    ]

                Bounce bounces _ ->
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
                                    (.position <| handCardPosition ctx PlayerB handIndex finalN hover)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.zRotation (handCardRotation PlayerB handIndex finalN))
                            , scale = Card.scale
                            , discarding = False
                            , mCard = Just card
                            , index = handIndex
                            }
                    in
                    List.map makeBounceEntity playerBounces

                BounceDeck bounces _ ->
                    let
                        playerBounces : List DeckBounce
                        playerBounces =
                            Animation.getPlayerBounceDeckCards PlayerB bounces model.stack

                        makeBounceEntity : DeckBounce -> OtherHandEntity
                        makeBounceEntity { stackIndex, card } =
                            let
                                stackEntity =
                                    Stack.Entities.stackEntity ctx stackIndex
                            in
                            { index = 0 -- ignore
                            , position =
                                interp progress
                                    stackEntity.position
                                    (vec3 -(1 + 0.1 * toFloat stackIndex) 1 -1)
                            , rotation =
                                Quaternion.lerp
                                    progress
                                    stackEntity.rotation
                                    (Quaternion.zRotation pi)
                            , scale = Card.scale
                            , discarding = False
                            , mCard = Just card
                            }
                    in
                    List.map makeBounceEntity playerBounces

                DiscardHand PlayerB discards ->
                    let
                        cardDiscards =
                            List.filterMap
                                (\( index, discard ) ->
                                    case discard of
                                        NoDiscard _ _ ->
                                            Nothing

                                        CardDiscard card ->
                                            Just ( index, card )
                                )
                            <|
                                List.indexedMap Tuple.pair
                                    discards

                        makeDiscardEntity : ( Int, KnowableCard ) -> OtherHandEntity
                        makeDiscardEntity ( index, _ ) =
                            let
                                initialPosition =
                                    .position <| handCardPosition ctx PlayerB index (n + List.length cardDiscards) hover

                                finalPosition =
                                    Math.Vector3.add initialPosition (vec3 0 -0.1 0.05)
                            in
                            { index = index
                            , position = interp progress initialPosition finalPosition
                            , rotation = Quaternion.zRotation (handCardRotation PlayerB index (n + List.length cardDiscards))
                            , scale = Card.scale
                            , mCard = Nothing
                            , discarding = True
                            }
                    in
                    List.map makeDiscardEntity cardDiscards

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
            pi - magnitude


handCardPosition : Context -> WhichPlayer -> Int -> Int -> Hover a -> { position : Vec3, hoverVector : Vec3 }
handCardPosition ctx which index count hover =
    let
        sign =
            case which of
                PlayerA ->
                    1

                PlayerB ->
                    -1

        scale =
            -0.004

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

                baseY =
                    abs <| 4 * (toFloat <| ceiling (i - (c * 0.5)))
            in
            sign * baseY

        hoverY =
            case ( which, hover ) of
                ( PlayerA, HoverHand hoverHand ) ->
                    if index == hoverHand.index then
                        interpFloat (hoverHand.tick / 70) 0 -10

                    else
                        0

                ( PlayerB, HoverOtherHand hoverOtherHand ) ->
                    if index == hoverOtherHand.index then
                        interpFloat (hoverOtherHand.tick / 70) 0 -10

                    else
                        0

                _ ->
                    0
    in
    { position =
        vec3 -x (scale * y) 0
            |> Math.Vector3.add
                (handOrigin ctx which count)
    , hoverVector = vec3 0 (sign * scale * hoverY) 0
    }


playPosition : Context -> Vec3
playPosition ctx =
    let
        entity =
            Stack.Entities.wheelEntity ctx (Stack.Entities.baseDistance ctx) 0 0
    in
    entity.position


applyHoverVector : { a | position : Vec3, hoverVector : Vec3 } -> { a | position : Vec3, hoverVector : Vec3 }
applyHoverVector entity =
    { entity | position = Math.Vector3.add entity.position entity.hoverVector }
