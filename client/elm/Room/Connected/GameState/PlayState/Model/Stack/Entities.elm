module Stack.Entities exposing (entities)

import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Game.Types exposing (Context)
import Math.Vector2 exposing (Vec2, vec2)
import Stack.Types exposing (Stack, StackCard)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : Context -> List (Card.Entity {})
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

                _ ->
                    finalStack

        mainEntities =
            List.indexedMap
                (clockEntity ctx rotationProgress)
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


clockEntity : Context -> Float -> Int -> StackCard -> Card.Entity {}
clockEntity { w, h, radius } progress index { card, owner } =
    let
        origin =
            vec2 (w / 2) (h / 2)

        i =
            toFloat index + 1.0

        segmentAngle : Float
        segmentAngle =
            -2.0 * pi / 12.0

        rotation : Float
        rotation =
            i * segmentAngle - progress * segmentAngle

        position : Vec2
        position =
            Math.Vector2.add origin <|
                Math.Vector2.scale (-0.615 * radius) <|
                    vec2 (sin rotation) (cos rotation)
    in
    { owner = owner
    , card = card
    , position = position
    , rotation = pi - rotation
    , scale = 1.3
    }
