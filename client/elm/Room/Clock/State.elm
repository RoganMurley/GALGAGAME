module Clock.State exposing (..)

import Animation.State
import Animation.Types exposing (Anim(..))
import Card.Types exposing (Card)
import Clock.Messages exposing (Msg(..))
import Clock.Types exposing (ClockParams, GameEntity, Model, Uniforms)
import Ease
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (Mat4, identity, makeLookAt, makeOrtho, makeRotate, mul)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.State as Model
import Raymarch.Types exposing (Height, Width)
import Resolvable.State exposing (activeAnim, activeModel)
import Stack.Types exposing (Stack, StackCard)
import WebGL exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Resolvable.State as Resolvable


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
        , entities = []
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

        entities =
            stackEntities
                { w = toFloat w, h = toFloat h, radius = radius }
                resModel.stack
                resInfo
    in
        { model
            | res = newRes
            , entities = entities
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
                    | focus = getFocus model pos
                    , mouse = pos
                }


getFocus : Model -> Vec2 -> Maybe Card
getFocus { entities } mouse =
    let
        hitTest : { a | position : Vec2 } -> Bool
        hitTest { position } =
            let
                dist =
                    Math.Vector2.distance position mouse
            in
                dist < 64

        hit =
            List.any hitTest entities
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


stackEntities : ClockParams -> Stack -> Maybe ( Float, Maybe Anim ) -> List (GameEntity { owner : WhichPlayer })
stackEntities { w, h, radius } finalStack resInfo =
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
