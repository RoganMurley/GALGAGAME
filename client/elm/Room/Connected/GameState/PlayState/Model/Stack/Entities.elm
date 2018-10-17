module Stack.Entities exposing (..)

import Animation.Types exposing (Anim(..))
import Card.Types as Card exposing (Card)
import Game.Types exposing (Context)
import Math.Vector2 exposing (Vec2, vec2)
import Game.Entity as Game
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
            clockFace
                stack
                (vec2 (w / 2) (h / 2))
                (0.615 * radius)
                rotationProgress

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


clockFace : Stack -> Vec2 -> Float -> Float -> List (Card.Entity {})
clockFace stack origin radius progress =
    let
        segments : Int
        segments =
            12

        genPoint : Int -> StackCard -> Game.Entity { card : Card, owner : WhichPlayer }
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
