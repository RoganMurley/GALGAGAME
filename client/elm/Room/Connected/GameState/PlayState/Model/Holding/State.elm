module Holding.State exposing (getHandIndex, init, tick)

import Card.Types exposing (Card)
import Collision
import Holding.Types exposing (Holding(..))
import Math.Vector3
import Quaternion


init : Card -> Int -> Maybe Collision.Ray -> Holding
init card handIndex mRay =
    case mRay of
        Just { origin, direction } ->
            Holding
                { card = card
                , handIndex = handIndex
                , pos =
                    Math.Vector3.add origin direction
                , rot = Quaternion.identity
                }

        Nothing ->
            NoHolding


getHandIndex : Holding -> Maybe Int
getHandIndex holding =
    case holding of
        NoHolding ->
            Nothing

        Holding { handIndex } ->
            Just handIndex


tick : Holding -> Maybe Collision.Ray -> Float -> Holding
tick holding ray _ =
    case holding of
        Holding oldHolding ->
            case ray of
                Just { origin, direction } ->
                    let
                        pos =
                            Math.Vector3.add origin direction

                        xDiff =
                            Math.Vector3.getX pos - Math.Vector3.getX oldHolding.pos

                        yDiff =
                            Math.Vector3.getY pos - Math.Vector3.getY oldHolding.pos

                        rot =
                            Quaternion.rotate
                                oldHolding.rot
                            <|
                                Quaternion.rotate
                                    (Quaternion.yRotation (5 * xDiff))
                                    (Quaternion.xRotation (5 * yDiff))
                    in
                    Holding
                        { card = oldHolding.card
                        , handIndex = oldHolding.handIndex
                        , pos = pos
                        , rot = Quaternion.lerp 0.1 rot Quaternion.identity
                        }

                Nothing ->
                    NoHolding

        NoHolding ->
            NoHolding
