module Clock.State exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Card exposing (CardEntity)
import Clock.Types exposing (ClockParams, GameEntity, Model)
import Hand.Types exposing (Hand)
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2, vec2)
import Maybe.Extra as Maybe
import Resolvable.State as Resolvable exposing (activeAnim, activeModel, activeStackCard)
import Resolvable.Types as Resolvable
import Stack.Types exposing (Stack, StackCard)
import Util exposing (floatInterp, interp2D)
import WhichPlayer.Types exposing (WhichPlayer(..))


clockInit : Resolvable.Model -> Model
clockInit res =
    { focus = Nothing
    , mouse = vec2 0 0
    , entities = { hand = [], otherHand = [], stack = [] }
    , res = res
    }


tick : Flags -> Model -> Float -> Model
tick { dimensions } ({ res } as model) dt =
    let
        resModel =
            activeModel res

        stackCard =
            activeStackCard res

        ( w, h ) =
            dimensions

        radius =
            if h < w then
                0.8 * (toFloat h / 2)
            else
                1.2 * (toFloat w / 2)

        resInfo =
            Just ( res.tick, activeAnim res )

        params =
            { w = toFloat w, h = toFloat h, radius = radius }

        stackEntities =
            calcStackEntities
                params
                resModel.stack
                stackCard
                resInfo

        handEntities =
            calcHandEntities
                params
                resModel.hand
                resInfo

        otherHandEntities =
            calcOtherHandEntities
                params
                resModel.otherHand
                resInfo
    in
        { model
            | res = Resolvable.tick dt res
            , entities =
                { stack = stackEntities
                , hand = handEntities
                , otherHand = otherHandEntities
                }
            , focus = getFocus model
        }


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


getFocus : Model -> Maybe StackCard
getFocus { entities, mouse, res } =
    let
        hoverCard =
            Maybe.or
                (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) <|
                    List.find (hitTest mouse 64) entities.stack
                )
                (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) <|
                    List.find (hitTest mouse 28) entities.hand
                )
    in
        Maybe.or (activeStackCard res) hoverCard


calcStackEntities : ClockParams -> Stack -> Maybe StackCard -> Maybe ( Float, Maybe Anim ) -> List (CardEntity {})
calcStackEntities { w, h, radius } finalStack stackCard resInfo =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        progress =
            Animation.progress anim resTick

        rotationProgress =
            case anim of
                Just (Rotate _) ->
                    progress

                Just (Windup _) ->
                    1 - progress

                _ ->
                    0

        stack : Stack
        stack =
            case anim of
                Just (Play _ _ _) ->
                    List.drop 1 finalStack

                Just (Rotate _) ->
                    case stackCard of
                        Just c ->
                            c :: finalStack

                        Nothing ->
                            finalStack

                _ ->
                    finalStack

        entities =
            clockFace
                stack
                (vec2 (w / 2) (h / 2))
                (0.615 * radius)
                rotationProgress

        extraEntities =
            case anim of
                Just (Rotate _) ->
                    []

                _ ->
                    case stackCard of
                        Just { owner, card } ->
                            [ { owner = owner
                              , card = card
                              , position =
                                    Math.Vector2.add
                                        (vec2 (w / 2) (h / 2))
                                    <|
                                        vec2 0 (-0.615 * radius)
                              , rotation = pi
                              , scale = 1.3
                              }
                            ]

                        Nothing ->
                            []
    in
        entities ++ extraEntities


handOrigin : ClockParams -> WhichPlayer -> Int -> Vec2
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


handCardPosition : ClockParams -> WhichPlayer -> Int -> Int -> Vec2
handCardPosition ({ radius } as params) which index count =
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
            (handOrigin params which count)
        <|
            Math.Vector2.add
                (vec2 (toFloat index * (width + spacing)) 0)
            <|
                vec2 0 y


calcHandEntities : ClockParams -> Hand -> Maybe ( Float, Maybe Anim ) -> List (CardEntity { index : Int })
calcHandEntities ({ w, h, radius } as params) finalHand resInfo =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        ( hand, drawingCard ) =
            case anim of
                Just (Draw PlayerA) ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                _ ->
                    ( finalHand, Nothing )

        indexModifier : Int -> Int
        indexModifier =
            case anim of
                Just (Play PlayerA _ index) ->
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

        progress =
            Animation.progress anim resTick

        playProgress =
            progress

        entity : ( Int, Card ) -> CardEntity { index : Int }
        entity ( finalI, card ) =
            let
                i =
                    indexModifier finalI

                pos =
                    interp2D progress
                        (handCardPosition params PlayerA i n)
                        (handCardPosition params PlayerA finalI finalN)

                rot =
                    floatInterp progress
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

        mainEntities : List (CardEntity { index : Int })
        mainEntities =
            List.map entity <| List.indexedMap (,) hand

        extraEntities : List (CardEntity { index : Int })
        extraEntities =
            case anim of
                Just (Draw PlayerA) ->
                    case drawingCard of
                        Just card ->
                            let
                                pos =
                                    interp2D progress
                                        (vec2 w h)
                                        (handCardPosition params PlayerA n (n + 1))

                                rot =
                                    floatInterp progress 0 (handCardRotation PlayerA n (n + 1))
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

                Just (Play PlayerA card i) ->
                    let
                        pos =
                            interp2D playProgress
                                (handCardPosition params PlayerA i n)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))

                        rot =
                            floatInterp playProgress (handCardRotation PlayerA i n) pi

                        scale =
                            floatInterp playProgress 1 1.3
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


calcOtherHandEntities : ClockParams -> Int -> Maybe ( Float, Maybe Anim ) -> List (GameEntity {})
calcOtherHandEntities ({ w, h, radius } as params) finalN resInfo =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        n =
            case anim of
                Just (Draw PlayerB) ->
                    finalN - 1

                _ ->
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

                _ ->
                    Basics.identity

        progress =
            Animation.progress anim resTick

        entity : Int -> GameEntity {}
        entity finalI =
            let
                i =
                    indexModifier finalI
            in
                { position =
                    interp2D progress
                        (handCardPosition params PlayerB i n)
                        (handCardPosition params PlayerB finalI finalN)
                , rotation =
                    floatInterp progress
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
                Just (Draw PlayerB) ->
                    [ { position =
                            interp2D progress
                                (vec2 w 0)
                                (handCardPosition params PlayerB n (n + 1))
                      , rotation =
                            floatInterp progress 0 (handCardRotation PlayerB n (n + 1))
                      , scale = 1
                      }
                    ]

                Just (Play PlayerB _ i) ->
                    [ { position =
                            interp2D progress
                                (handCardPosition params PlayerB i n)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))
                      , rotation =
                            floatInterp progress (handCardRotation PlayerB i n) 0
                      , scale =
                            floatInterp progress 1 1.3
                      }
                    ]

                _ ->
                    []
    in
        mainEntities ++ extraEntities


clockFace : Stack -> Vec2 -> Float -> Float -> List (CardEntity {})
clockFace stack origin radius progress =
    let
        segments : Int
        segments =
            12

        genPoint : Int -> StackCard -> GameEntity { card : Card, owner : WhichPlayer }
        genPoint index { card, owner } =
            let
                i =
                    index + 1
            in
                { owner = owner
                , card = card
                , position = Math.Vector2.add origin <| offset i
                , rotation = rotation i
                , scale = 1.3
                }

        segmentAngle : Float
        segmentAngle =
            -2.0 * pi / toFloat segments

        rot : Int -> Float
        rot i =
            (toFloat i * segmentAngle)
                - (progress * segmentAngle)

        offset : Int -> Vec2
        offset i =
            Math.Vector2.scale -radius <|
                vec2 (sin <| rot i) (cos <| rot i)

        rotation : Int -> Float
        rotation i =
            pi - rot i
    in
        List.indexedMap genPoint stack
