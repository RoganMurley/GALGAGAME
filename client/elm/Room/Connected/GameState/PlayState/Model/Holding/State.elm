module Holding.State exposing (getHandIndex, init, tick)

import Card.Types exposing (Card)
import Collision
import Holding.Types exposing (Holding(..))
import Math.Vector3


init : Card -> Int -> Maybe Collision.Ray -> Holding
init card handIndex mRay =
    case mRay of
        Just { origin, direction } ->
            Holding card handIndex <|
                Math.Vector3.add origin direction

        Nothing ->
            NoHolding


getHandIndex : Holding -> Maybe Int
getHandIndex holding =
    case holding of
        NoHolding ->
            Nothing

        Holding _ index _ ->
            Just index


tick : Holding -> Maybe Collision.Ray -> Float -> Holding
tick holding ray _ =
    case holding of
        Holding card handIndex _ ->
            init card handIndex ray

        NoHolding ->
            NoHolding
