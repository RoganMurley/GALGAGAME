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


entities : Context -> List StackEntity
entities ctx =
    let
        { anim, model, progress, stackCard } =
            ctx

        finalStack =
            model.stack

        rotationProgress =
            case anim of
                Rotate _ ->
                    progress

                Windup _ ->
                    1 - progress

                _ ->
                    0

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
                (stackCardEntity ctx rotationProgress (List.length stack))
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


stackCardEntity : Context -> Float -> Int -> Int -> StackCard -> StackEntity
stackCardEntity ctx baseRotateProgress finalStackLen finalIndex { card, owner } =
    let
        entity : Game.Entity3D {}
        entity =
            stackEntity ctx baseRotateProgress finalStackLen finalIndex
    in
    { owner = owner
    , card = card
    , index = finalIndex
    , position = entity.position
    , rotation = entity.rotation
    , scale = entity.scale
    }


stackEntity : Context -> Float -> Int -> Int -> Game.Entity3D {}
stackEntity { anim, progress } baseRotateProgress finalStackLen finalIndex =
    let
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

        animRotateProgress : Float
        animRotateProgress =
            case anim of
                Fabricate _ ->
                    if finalI < 2 then
                        0

                    else
                        0

                _ ->
                    0

        rotateProgress : Float
        rotateProgress =
            baseRotateProgress + animRotateProgress

        ringRotation : Float
        ringRotation =
            interpFloat progress
                (i * segmentAngle - rotateProgress * segmentAngle)
                (finalI * segmentAngle - rotateProgress * segmentAngle)

        position : Vec3
        position =
            Math.Vector3.scale distance <|
                vec3 (sin ringRotation) (cos ringRotation) 0

        rotation : Quaternion
        rotation =
            baseRotation
                |> Quaternion.rotate (Quaternion.zRotation -ringRotation)

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
    { position = position
    , rotation = rotation
    , scale = Card.scale
    }


baseDistance : Float
baseDistance =
    0.5


baseRotation : Quaternion
baseRotation =
    Quaternion.xRotation (-0.35 * pi)


segmentAngle : Float
segmentAngle =
    -2.0 * pi / 12.0


wheelEntities : Context -> List WheelEntity
wheelEntities ctx =
    let
        { anim, progress } =
            ctx

        rotateProgress =
            case anim of
                Rotate _ ->
                    progress

                Windup _ ->
                    1 - progress

                _ ->
                    0

        distance : Float
        distance =
            baseDistance

        wheelEntity : Float -> WheelEntity
        wheelEntity i =
            let
                position : Vec3
                position =
                    Math.Vector3.scale distance <|
                        vec3 (sin ringRotation) (cos ringRotation) 0

                ringRotation : Float
                ringRotation =
                    i * segmentAngle - rotateProgress * segmentAngle

                rotation : Quaternion
                rotation =
                    baseRotation
                        |> Quaternion.rotate (Quaternion.zRotation -ringRotation)
            in
            { position = position
            , rotation = rotation
            , scale = Card.scale
            , alpha = 0.3
            }
    in
    List.map wheelEntity <| List.map toFloat <| List.range 0 11
