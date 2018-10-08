module Clock.State exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Card exposing (CardEntity)
import Clock.Types exposing (Context, GameEntity, Model)
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2, vec2)
import Maybe.Extra as Maybe
import Resolvable.State as Resolvable exposing (activeAnim, activeModel, activeStackCard)
import Resolvable.Types as Resolvable
import Stack.Types exposing (Stack, StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (interpFloat, interp2D)
import WhichPlayer.Types exposing (WhichPlayer(..))


clockInit : Resolvable.Model -> Model
clockInit res =
    { focus = Nothing
    , mouse = vec2 0 0
    , entities = { hand = [], otherHand = [], stack = [] }
    , res = res
    }


contextInit : ( Int, Int ) -> Resolvable.Model -> Texture.Model -> Context
contextInit ( width, height ) res textures =
    let
        w =
            toFloat width

        h =
            toFloat height

        radius =
            if h < w then
                0.8 * h * 0.5
            else
                1.2 * w * 0.5

        anim =
            activeAnim res
    in
        { w = w
        , h = h
        , radius = radius
        , anim = anim
        , model = activeModel res
        , stackCard = activeStackCard res
        , tick = res.tick
        , progress = Animation.progress anim res.tick
        , textures = textures
        }


tick : Flags -> Model -> Float -> Model
tick { dimensions } model dt =
    let
        ctx =
            contextInit dimensions model.res Texture.init
    in
        { model
            | res = Resolvable.tick dt model.res
            , entities =
                { stack =
                    calcStackEntities ctx
                , hand =
                    calcHandEntities ctx
                , otherHand =
                    calcOtherHandEntities ctx
                }
            , focus = getFocus ctx model
        }


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


getFocus : Context -> Model -> Maybe StackCard
getFocus { stackCard } { entities, mouse } =
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
        Maybe.or stackCard hoverCard


calcStackEntities : Context -> List (CardEntity {})
calcStackEntities ctx =
    let
        { w, h, radius, anim, model, progress, stackCard } =
            ctx

        finalStack =
            model.stack

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
                                        (vec2 0 (-0.615 * radius))
                              , rotation = pi
                              , scale = 1.3
                              }
                            ]

                        Nothing ->
                            []
    in
        entities ++ extraEntities


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


calcHandEntities : Context -> List (CardEntity { index : Int })
calcHandEntities ({ w, h, radius, anim, model, progress } as ctx) =
    let
        finalHand =
            model.hand

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

        entity : ( Int, Card ) -> CardEntity { index : Int }
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

                Just (Play PlayerA card i) ->
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


calcOtherHandEntities : Context -> List (GameEntity {})
calcOtherHandEntities ({ w, h, radius, anim, model, progress } as ctx) =
    let
        finalN =
            model.otherHand

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
                Just (Draw PlayerB) ->
                    [ { position =
                            interp2D progress
                                (vec2 w 0)
                                (handCardPosition ctx PlayerB n (n + 1))
                      , rotation =
                            interpFloat progress 0 (handCardRotation PlayerB n (n + 1))
                      , scale = 1
                      }
                    ]

                Just (Play PlayerB _ i) ->
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
