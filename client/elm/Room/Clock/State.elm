module Clock.State exposing (..)

import Animation.State
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Card exposing (CardEntity)
import Clock.Messages exposing (Msg(..))
import Clock.Types exposing (ClockParams, GameEntity, Model)
import Ease
import Hand.Types exposing (Hand)
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2, vec2)
import Maybe.Extra as Maybe
import Model.State as Model
import Resolvable.State exposing (activeAnim, activeModel, activeStackCard)
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Stack.Types exposing (Stack, StackCard)
import Util exposing (floatInterp, interp, interp2D)
import WhichPlayer.Types exposing (WhichPlayer(..))


clockInit : Resolvable.Model -> Model
clockInit res =
    { focus = Nothing
    , mouse = vec2 0 0
    , entities = { hand = [], otherHand = [], stack = [] }
    , res = res
    }


init : Model
init =
    let
        mInit =
            Model.init

        card =
            { name = "", desc = "", imgURL = "" }

        stackCardA =
            { owner = PlayerA, card = card }

        stackCardB =
            { stackCardA | owner = PlayerB }

        stackLen =
            12

        model =
            { mInit
                | hand = List.repeat 6 card
                , otherHand = 6
                , stack =
                    List.repeat (stackLen // 2) stackCardB
                        ++ List.repeat (stackLen // 2) stackCardA
            }
    in
        { focus = Just card
        , mouse = vec2 -10000 -10000
        , entities = { hand = [], otherHand = [], stack = [] }
        , res =
            Resolvable.init
                { model
                    | stack = []
                    , hand = []
                    , otherHand = 0
                }
            <|
                List.concat
                    [ [ { model =
                            { model
                                | hand = []
                                , otherHand = 0
                                , stack = []
                            }
                        , anim = Just (GameStart PlayerA)
                        , stackCard = Nothing
                        }
                      ]
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | hand = []
                                    , otherHand = i
                                    , stack = []
                                }
                            , anim = Just (Draw PlayerB)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 1 <| model.otherHand)
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | hand = List.drop (List.length model.hand - i) model.hand
                                    , stack = []
                                }
                            , anim = Just (Draw PlayerA)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 1 <| List.length model.hand)
                    , [ { model =
                            { model
                                | hand = model.hand
                                , stack = []
                            }
                        , anim = Just (Overdraw PlayerA card)
                        , stackCard = Nothing
                        }
                      ]
                    , [ { model =
                            { model
                                | hand = model.hand
                                , stack = []
                            }
                        , anim = Just (Overdraw PlayerB card)
                        , stackCard = Nothing
                        }
                      ]
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | stack = List.drop (List.length model.stack - i) model.stack
                                    , hand = List.drop (i + 1) model.hand
                                }
                            , anim = Just (Play PlayerA card 0)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 0 <| List.length model.hand - 1)
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | stack = List.drop (List.length model.stack - i - List.length model.hand) model.stack
                                    , hand = []
                                    , otherHand = model.otherHand - (i + 1)
                                }
                            , anim = Just (Play PlayerB card 0)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 0 (model.otherHand - 1))
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | stack = List.drop i model.stack
                                    , hand = []
                                    , otherHand = 0
                                }
                            , anim = Just (Rotate PlayerA)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 0 (stackLen - 1))
                    ]
        }


tick : Flags -> Model -> Float -> Model
tick { dimensions } ({ res } as model) dt =
    let
        newRes =
            if res.tick > animToResTickMax (Resolvable.activeAnim res) then
                Resolvable.resolveStep res
            else
                { res
                    | tick = res.tick + dt
                }

        resModel =
            activeModel res

        stackCard =
            activeStackCard res

        ( w, h ) =
            dimensions

        radius =
            0.8 * (toFloat h / 2)

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
            | res = newRes
            , entities =
                { stack = stackEntities
                , hand = handEntities
                , otherHand = otherHandEntities
                }
            , focus = getFocus model
        }


animToResTickMax : Maybe Anim -> Float
animToResTickMax anim =
    case anim of
        Just (Draw _) ->
            500

        Just (Play _ _ _) ->
            1000

        Just (Overdraw _ _) ->
            1000

        otherwise ->
            Animation.State.animToResTickMax anim


update : Model -> Msg -> Model
update model msg =
    case msg of
        Mouse { x, y } ->
            let
                pos =
                    vec2 (toFloat x) (toFloat y)
            in
                { model
                    | mouse = pos
                }


hitTest : Vec2 -> { a | position : Vec2 } -> Bool
hitTest pos { position } =
    let
        dist =
            Math.Vector2.distance position pos
    in
        dist < 64


getFocus : Model -> Maybe Card
getFocus { entities, mouse, res } =
    let
        resCard =
            Maybe.map .card <| activeStackCard res

        hoverCardHitTest =
            List.find <| hitTest mouse

        hoverCard =
            Maybe.or
                (Maybe.map .card <| hoverCardHitTest entities.stack)
                (Maybe.map .card <| hoverCardHitTest entities.hand)
    in
        Maybe.or resCard hoverCard


calcStackEntities : ClockParams -> Stack -> Maybe StackCard -> Maybe ( Float, Maybe Anim ) -> List (CardEntity {})
calcStackEntities { w, h, radius } finalStack stackCard resInfo =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        progress =
            case anim of
                Just (Rotate _) ->
                    Ease.inQuad <| resTick / maxTick

                Just (Play _ _ _) ->
                    1 - (Ease.inQuad <| resTick / maxTick)

                otherwise ->
                    0

        stack : Stack
        stack =
            case anim of
                Just (Rotate _) ->
                    case stackCard of
                        Just c ->
                            c :: finalStack

                        Nothing ->
                            finalStack

                Just (Play _ _ _) ->
                    List.drop 1 finalStack

                otherwise ->
                    finalStack

        entities =
            clockFace
                stack
                (vec2 (w / 2) (h / 2))
                (0.615 * radius)
                progress

        extraEntities =
            case anim of
                Just (Rotate _) ->
                    []

                otherwise ->
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
handCardPosition ({ w, h, radius } as params) which index count =
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
        Math.Vector2.add
            (handOrigin params which count)
        <|
            Math.Vector2.add
                (vec2 ((toFloat index) * (width + spacing)) 0)
            <|
                vec2 0 y


calcHandEntities : ClockParams -> Hand -> Maybe ( Float, Maybe Anim ) -> List (CardEntity { index : Int })
calcHandEntities ({ w, h, radius } as params) finalHand resInfo =
    let
        cardDimensions : ClockParams -> ( Float, Float, Float )
        cardDimensions { w, h, radius } =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        ( hand, drawingCard ) =
            case anim of
                Just (Draw PlayerA) ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                otherwise ->
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

                otherwise ->
                    Basics.identity

        n =
            List.length hand

        finalN =
            List.length finalHand

        progress =
            Ease.outQuint <| resTick / maxTick

        playProgress =
            Ease.inQuad <| resTick / maxTick

        ( width, height, spacing ) =
            cardDimensions params

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

                otherwise ->
                    []
    in
        mainEntities ++ extraEntities


calcOtherHandEntities : ClockParams -> Int -> Maybe ( Float, Maybe Anim ) -> List (GameEntity {})
calcOtherHandEntities ({ w, h, radius } as params) finalN resInfo =
    let
        cardDimensions : ClockParams -> ( Float, Float, Float )
        cardDimensions { w, h, radius } =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

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
                    Basics.identity

        progress =
            Ease.outQuint <| resTick / maxTick

        playProgress =
            Ease.inQuad <| resTick / maxTick

        ( width, height, spacing ) =
            cardDimensions params

        entity : Int -> GameEntity {}
        entity finalI =
            let
                i =
                    indexModifier finalI

                pos =
                    interp2D progress
                        (handCardPosition params PlayerB i n)
                        (handCardPosition params PlayerB finalI finalN)

                rot =
                    floatInterp progress
                        (handCardRotation PlayerB i n)
                        (handCardRotation PlayerB finalI finalN)
            in
                { position = pos
                , rotation = rot
                , scale = 1
                }

        mainEntities : List (GameEntity {})
        mainEntities =
            List.map entity (List.range 0 (n - 1))

        extraEntities : List (GameEntity {})
        extraEntities =
            case anim of
                Just (Draw PlayerB) ->
                    let
                        pos =
                            interp2D progress
                                (vec2 w 0)
                                (handCardPosition params PlayerB n (n + 1))

                        rot =
                            floatInterp progress 0 (handCardRotation PlayerB n (n + 1))
                    in
                        [ { position = pos
                          , rotation = rot
                          , scale = 1
                          }
                        ]

                Just (Play PlayerB _ i) ->
                    let
                        pos =
                            interp2D playProgress
                                (handCardPosition params PlayerB i n)
                                (vec2 (w / 2) (h / 2 - radius * 0.62))

                        rot =
                            floatInterp playProgress (handCardRotation PlayerB i n) 0

                        scale =
                            floatInterp playProgress 1 1.3
                    in
                        [ { position = pos
                          , rotation = rot
                          , scale = scale
                          }
                        ]

                otherwise ->
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
