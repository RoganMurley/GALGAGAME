module Stack.Entities exposing (baseDistance, baseRotation, entities, stackEntity, wheelEntities, wheelEntity)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..))
import Card.State as Card
import Game.Entity as Game
import Game.Types exposing (Context, StackEntity, WheelEntity)
import Math.Matrix4 exposing (rotate)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Quaternion exposing (Quaternion)
import Stack.Types exposing (Stack, StackCard)
import Util exposing (interpFloat)
import Wheel.State as Wheel


baseDistance : Context -> Float
baseDistance _ =
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
        { anim, model } =
            ctx

        finalStack =
            model.stack

        stack : Stack
        stack =
            case anim of
                Play _ _ _ _ ->
                    { finalStack | wheel0 = Nothing }

                _ ->
                    finalStack
    in
    Maybe.values <|
        Wheel.toList <|
            Wheel.map (\( i, mSc ) -> Maybe.map (stackCardEntity ctx i) mSc) <|
                Wheel.apply
                    (Wheel.map (\x y -> ( x, y )) Wheel.indexed)
                    stack


stackCardEntity : Context -> Int -> StackCard -> StackEntity
stackCardEntity ctx finalIndex { card, owner } =
    let
        entity : Game.Entity3D {}
        entity =
            stackEntity ctx finalIndex
    in
    { owner = owner
    , card = card
    , index = finalIndex
    , position = entity.position
    , rotation = entity.rotation
    , scale = entity.scale
    }


stackEntity : Context -> Int -> Game.Entity3D {}
stackEntity ctx finalIndex =
    let
        { anim, progress } =
            ctx

        finalI : Float
        finalI =
            case anim of
                MoveStack moves _ ->
                    case Wheel.get finalIndex moves |> Maybe.join of
                        Just moveIndex ->
                            toFloat moveIndex

                        _ ->
                            toFloat finalIndex

                Rotate _ ->
                    toFloat finalIndex + 1

                _ ->
                    toFloat finalIndex

        i : Float
        i =
            case anim of
                Rotate _ ->
                    finalI

                _ ->
                    toFloat finalIndex

        distance : Float
        distance =
            case anim of
                DiscardStack discards ->
                    case Wheel.get finalIndex discards of
                        Just True ->
                            baseDistance ctx + toFloat (12 - finalIndex) * progress * 0.01

                        _ ->
                            baseDistance ctx

                _ ->
                    baseDistance ctx
    in
    wheelEntity ctx distance i finalI


wheelEntities : Context -> List WheelEntity
wheelEntities ctx =
    List.map (\i -> wheelEntity ctx (baseDistance ctx) i i) <|
        List.map toFloat <|
            List.range 0 11


wheelEntity : Context -> Float -> Float -> Float -> WheelEntity
wheelEntity ctx distance i finalI =
    let
        { anim, progress, w, h } =
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

        z =
            if w >= h then
                0

            else if w / h < 0.5 then
                2.5

            else if w / h < 0.55 then
                2

            else if w / h < 0.6 then
                1.5

            else
                1.25

        position : Vec3
        position =
            Math.Vector3.scale distance <|
                vec3 (sin ringRotation) (cos ringRotation) z

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
