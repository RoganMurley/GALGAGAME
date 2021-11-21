module Holding.State exposing (getHandIndex, init, setDamage, tick)

import Card.Types exposing (Card)
import Collision
import Holding.Types exposing (Holding(..))
import Hover exposing (HoverDamage)
import Math.Vector3
import Quaternion


init : Card -> Int -> Maybe Collision.Ray -> ( HoverDamage, HoverDamage ) -> Holding
init card handIndex mRay dmg =
    case mRay of
        Just { origin, direction } ->
            Holding
                { card = card
                , handIndex = handIndex
                , dmg = dmg
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
                        , dmg = oldHolding.dmg
                        , pos = pos
                        , rot = Quaternion.lerp 0.1 rot Quaternion.identity
                        }

                Nothing ->
                    NoHolding

        NoHolding ->
            NoHolding


setDamage : Holding -> ( HoverDamage, HoverDamage ) -> Holding
setDamage holding dmg =
    case holding of
        Holding h ->
            Holding { h | dmg = dmg }

        NoHolding ->
            NoHolding
