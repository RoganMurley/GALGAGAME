module Stack.Entities exposing (entities, stackEntity)

import Animation.Types exposing (Anim(..), Bounce(..))
import Array
import Game.Entity as Game
import Game.Types exposing (Context, StackEntity)
import Math.Vector2 exposing (Vec2, vec2)
import Random
import Random.List
import Stack.Types exposing (Stack, StackCard)
import Util exposing (interpFloat)


entities : Context -> List StackEntity
entities ctx =
    let
        { w, h, radius, anim, model, progress, stackCard } =
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
                Play _ _ _ ->
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
    mainEntities ++ extraEntities


stackCardEntity : Context -> Float -> Int -> Int -> StackCard -> StackEntity
stackCardEntity ctx baseRotateProgress finalStackLen finalIndex { card, owner } =
    let
        entity : Game.Entity {}
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


stackEntity : Context -> Float -> Int -> Int -> Game.Entity {}
stackEntity { w, h, radius, anim, progress } baseRotateProgress finalStackLen finalIndex =
    let
        origin : Vec2
        origin =
            vec2 (w / 2) (h / 2)

        finalI : Float
        finalI =
            case anim of
                Bounce bounces ->
                    case Array.get finalIndex <| Array.fromList bounces of
                        Just (NoBounce bounceIndex) ->
                            toFloat bounceIndex + 1.0

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

                _ ->
                    finalI

        stackLen : Int
        stackLen =
            case anim of
                Fabricate _ ->
                    finalStackLen - 1

                _ ->
                    finalStackLen

        segmentAngle : Float
        segmentAngle =
            -2.0 * pi / 12.0

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

        rotation : Float
        rotation =
            interpFloat progress
                (i * segmentAngle - rotateProgress * segmentAngle)
                (finalI * segmentAngle - rotateProgress * segmentAngle)

        position : Vec2
        position =
            Math.Vector2.add origin <|
                Math.Vector2.scale (-0.615 * radius) <|
                    vec2 (sin rotation) (cos rotation)
    in
    { position = position
    , rotation = pi - rotation
    , scale = 1.3
    }
