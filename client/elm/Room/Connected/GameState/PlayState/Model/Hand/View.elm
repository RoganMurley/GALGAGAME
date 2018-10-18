module Hand.View exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Card.View as Card
import Game.Types exposing (Context)
import Ease
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Game.Entity as Game
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (interpFloat, interp2D)


cardDimensions : Context -> { width : Float, height : Float, spacing : Float }
cardDimensions { radius } =
    { width = 0.1 * radius
    , height = 0.1 * radius
    , spacing = 35.0
    }


origin : Context -> WhichPlayer -> Int -> Vec3
origin ({ w, h } as ctx) which count =
    let
        { width, height, spacing } =
            cardDimensions ctx

        x =
            w / 2 - 0.5 * (width + spacing) * (toFloat <| count - 1)

        y =
            case which of
                PlayerA ->
                    h - height

                PlayerB ->
                    height
    in
        vec3 x y 0


rotation : WhichPlayer -> Int -> Int -> Float
rotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
        case which of
            PlayerA ->
                pi + magnitude

            PlayerB ->
                -magnitude


position : Context -> WhichPlayer -> Int -> Int -> Vec3
position ctx which index count =
    let
        { width, spacing } =
            cardDimensions ctx

        sign =
            case which of
                PlayerA ->
                    1

                PlayerB ->
                    -1

        y =
            let
                i =
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                sign * (abs <| 4 * (toFloat <| ceiling (i - c * 0.5)))
    in
        Math.Vector3.add
            (origin ctx which count)
        <|
            Math.Vector3.add
                (vec3 (toFloat index * (width + spacing)) 0 0)
            <|
                vec3 0 y 0


view : List (Card.Entity { index : Int }) -> Context -> List WebGL.Entity
view handEntities ctx =
    List.concat <|
        List.map (Card.view ctx) handEntities


otherView : List (Game.Entity {}) -> Context -> List WebGL.Entity
otherView otherHandEntities ctx =
    List.map (Card.backView ctx) otherHandEntities


millView : Context -> List WebGL.Entity
millView ({ w, h, progress, tick, anim } as ctx) =
    case anim of
        Mill owner card ->
            let
                sign =
                    case owner of
                        PlayerA ->
                            1

                        PlayerB ->
                            -1

                startPos =
                    case owner of
                        PlayerA ->
                            vec2 w h

                        PlayerB ->
                            vec2 w 0

                entity =
                    { owner = owner
                    , card = card
                    , position =
                        interp2D
                            progress
                            startPos
                            (vec2 (w / 2) (h / 2))
                    , rotation = interpFloat progress pi (pi - sign * 0.05 * pi)
                    , scale = interpFloat progress 1 4
                    }
            in
                Card.dissolvingView
                    { ctx
                        | progress =
                            Ease.inQuint (tick / Animation.animMaxTick anim)
                    }
                    entity

        _ ->
            []
