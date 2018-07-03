module Clock.State exposing (..)

import Animation.State
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Messages exposing (Msg(..))
import Clock.Types exposing (ClockParams, GameEntity, Model, Uniforms)
import Ease
import Hand.Types exposing (Hand)
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (Mat4, identity, makeLookAt, makeOrtho, makeRotate, mul)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.State as Model
import Raymarch.Types exposing (Height, Width)
import Resolvable.State exposing (activeAnim, activeModel)
import Resolvable.State as Resolvable
import Stack.Types exposing (Stack, StackCard)
import Util exposing (floatInterp, interp, interp2D)
import WebGL exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))


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
        , entities = { hand = [], stack = [] }
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
                    , List.map
                        (\i ->
                            { model =
                                { model
                                    | hand = model.hand
                                    , stack = []
                                }
                            , anim = Just (Overdraw PlayerA card)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 1 5)
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

        ( w, h ) =
            dimensions

        radius =
            0.8 * (toFloat h / 2)

        resInfo =
            Just ( res.tick, activeAnim res )

        stackEntities =
            calcStackEntities
                { w = toFloat w, h = toFloat h, radius = radius }
                resModel.stack
                resInfo

        handEntities =
            calcHandEntities
                { w = toFloat w, h = toFloat h, radius = radius }
                resModel.hand
                resInfo
    in
        { model
            | res = newRes
            , entities =
                { stack = stackEntities
                , hand = handEntities
                }
            , focus = getFocus model
        }


uniforms : Float -> ( Width, Height ) -> Texture -> Vec3 -> Mat4 -> Mat4 -> Vec3 -> Uniforms {}
uniforms t ( width, height ) texture pos rot scale color =
    { resolution = vec2 (toFloat width) (toFloat height)
    , texture = texture
    , rotation = rot
    , scale = scale
    , color = color
    , worldPos = pos
    , worldRot = makeRotate 0 (vec3 0 0 1)
    , perspective = makeOrtho 0 (toFloat width / 2) (toFloat height / 2) 0 0.01 1000
    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
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
getFocus { entities, mouse } =
    let
        hit =
            (List.any (hitTest mouse) <| entities.stack)
                || (List.any (hitTest mouse) <| entities.hand)
    in
        if hit then
            (Just
                { name = "Sword"
                , desc = "Hurt for 10"
                , imgURL = ""
                }
            )
        else
            Nothing


calcStackEntities : ClockParams -> Stack -> Maybe ( Float, Maybe Anim ) -> List (GameEntity { owner : WhichPlayer })
calcStackEntities { w, h, radius } finalStack resInfo =
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
                    List.drop 1 finalStack

                otherwise ->
                    finalStack

        entities =
            clockFace
                stack
                (vec2 (w / 2) (h / 2))
                (0.615 * radius)
                progress
    in
        entities


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


calcHandEntities : ClockParams -> Hand -> Maybe ( Float, Maybe Anim ) -> List (GameEntity {})
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

        hand =
            case anim of
                Just (Draw PlayerA) ->
                    List.take (List.length finalHand - 1) finalHand

                otherwise ->
                    finalHand

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

        ( width, height, spacing ) =
            cardDimensions params

        entity : Int -> GameEntity {}
        entity finalI =
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
                }

        mainEntities : List (GameEntity {})
        mainEntities =
            List.map entity (List.range 0 (n - 1))

        extraEntities =
            []
    in
        mainEntities ++ extraEntities


clockFace : Stack -> Vec2 -> Float -> Float -> List (GameEntity { owner : WhichPlayer })
clockFace stack origin radius progress =
    let
        segments : Int
        segments =
            12

        genPoint : Int -> StackCard -> GameEntity { owner : WhichPlayer }
        genPoint index { owner } =
            let
                i =
                    index + 1
            in
                { owner = owner
                , position = Math.Vector2.add origin <| offset i
                , rotation = rotation i
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
            2 * pi - rot i
    in
        List.indexedMap genPoint stack
