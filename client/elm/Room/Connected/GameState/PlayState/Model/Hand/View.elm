module Hand.View exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Card.View as Card
import Game.Types exposing (Context)
import Ease
import Math.Vector2 exposing (vec2)
import Game.Entity as Game
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (interpFloat, interp2D)


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
