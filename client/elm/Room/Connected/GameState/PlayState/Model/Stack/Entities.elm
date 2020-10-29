module Stack.Entities exposing (baseDistance, baseRotation, entities, stackEntity, wheelEntities)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), CardLimbo(..))
import Array
import Card.State as Card
import Game.Entity as Game
import Game.Types exposing (Context, StackEntity, WheelEntity)
import Math.Matrix4 exposing (rotate)
import Math.Vector3 exposing (Vec3, vec3)
import Quaternion exposing (Quaternion)
import Random
import Random.List
import Stack.Types exposing (Stack, StackCard)
import Util exposing (interpFloat)


baseDistance : Float
baseDistance =
    0.5


baseRotation : Quaternion
baseRotation =
    Quaternion.xRotation (-0.35 * pi)


segmentAngle : Float
segmentAngle =
    -2.0 * pi / 12.0


entities : Context -> List StackEntity
entities ctx =
    let
        { anim, model, progress, stackCard } =
            ctx

        finalStack =
            model.stack

        stack : Stack
        stack =
            case anim of
                Play _ _ _ _ ->
                    List.drop 1 finalStack

                Rotate _ ->
                    case stackCard of
                        Just c ->
                            c :: finalStack

                        Nothing ->
                            finalStack

                Confound _ ->
                    let
                        generator : Random.Generator Stack
                        generator =
                            Random.List.shuffle finalStack

                        -- We get a new seed each 20% of the animation's progress.
                        -- So we shuffle the list 5 times.
                        seed : Random.Seed
                        seed =
                            Random.initialSeed <| floor <| progress / 0.2

                        ( shuffledStack, _ ) =
                            Random.step generator seed
                    in
                    shuffledStack

                _ ->
                    finalStack

        mainEntities =
            List.indexedMap
                (stackCardEntity ctx (List.length stack))
                stack

        extraEntities =
            case anim of
                Rotate _ ->
                    []

                _ ->
                    case stackCard of
                        Just { owner, card } ->
                            [ { owner = owner
                              , card = card
                              , index = -1
                              , position = vec3 0 baseDistance 0
                              , rotation = baseRotation
                              , scale = Card.scale
                              }
                            ]

                        Nothing ->
                            []
    in
    mainEntities ++ extraEntities


stackCardEntity : Context -> Int -> Int -> StackCard -> StackEntity
stackCardEntity ctx finalStackLen finalIndex { card, owner } =
    let
        entity : Game.Entity3D {}
        entity =
            stackEntity ctx finalStackLen finalIndex
    in
    { owner = owner
    , card = card
    , index = finalIndex
    , position = entity.position
    , rotation = entity.rotation
    , scale = entity.scale
    }


stackEntity : Context -> Int -> Int -> Game.Entity3D {}
stackEntity ctx finalStackLen finalIndex =
    let
        { anim, progress } =
            ctx

        finalI : Float
        finalI =
            case anim of
                Bounce bounces ->
                    case Array.get finalIndex <| Array.fromList bounces of
                        Just (NoBounce bounceIndex) ->
                            toFloat bounceIndex + 1.0

                        _ ->
                            toFloat finalIndex + 1.0

                DiscardStack discards ->
                    case Array.get finalIndex <| Array.fromList discards of
                        Just (NoDiscard discardIndex) ->
                            toFloat discardIndex + 1.0

                        _ ->
                            toFloat finalIndex + 1.0

                Limbo limbos ->
                    case Array.get finalIndex <| Array.fromList limbos of
                        Just (NoLimbo limboIndex) ->
                            toFloat limboIndex + 1.0

                        _ ->
                            toFloat finalIndex + 1.0

                _ ->
                    toFloat finalIndex + 1.0

        i : Float
        i =
            case anim of
                Reverse _ ->
                    if finalI == 0 then
                        0

                    else
                        1.0 + toFloat stackLen - finalI

                Fabricate _ ->
                    if finalI > 1 then
                        finalI - 1

                    else
                        finalI

                Bounce _ ->
                    toFloat finalIndex + 1.0

                DiscardStack _ ->
                    toFloat finalIndex + 1.0

                Limbo _ ->
                    toFloat finalIndex + 1.0

                _ ->
                    finalI

        stackLen : Int
        stackLen =
            case anim of
                Fabricate _ ->
                    finalStackLen - 1

                _ ->
                    finalStackLen

        distance : Float
        distance =
            case anim of
                DiscardStack discards ->
                    case Array.get finalIndex <| Array.fromList discards of
                        Just CardDiscard ->
                            baseDistance + toFloat (12 - finalIndex) * progress * 0.01

                        _ ->
                            baseDistance

                Limbo limbos ->
                    case Array.get finalIndex <| Array.fromList limbos of
                        Just CardLimbo ->
                            0.615 + toFloat (12 - finalIndex) * progress * 0.01

                        _ ->
                            baseDistance

                _ ->
                    baseDistance
    in
    wheelEntity ctx distance i finalI


wheelEntities : Context -> List WheelEntity
wheelEntities ctx =
    List.map (\i -> wheelEntity ctx baseDistance i i) <|
        List.map toFloat <|
            List.range 0 11


wheelEntity : Context -> Float -> Float -> Float -> WheelEntity
wheelEntity ctx distance i finalI =
    let
        { anim, progress } =
            ctx

        rotateProgress : Float
        rotateProgress =
            case anim of
                Rotate _ ->
                    progress

                Windup _ ->
                    1 - progress

                _ ->
                    0

        position : Vec3
        position =
            Math.Vector3.scale distance <|
                vec3 (sin ringRotation) (cos ringRotation) 0

        ringRotation : Float
        ringRotation =
            interpFloat progress
                (i * segmentAngle - rotateProgress * segmentAngle)
                (finalI * segmentAngle - rotateProgress * segmentAngle)

        rotation : Quaternion
        rotation =
            baseRotation
                |> Quaternion.rotate (Quaternion.zRotation -ringRotation)
    in
    { position = position
    , rotation = rotation
    , scale = Card.scale
    }
