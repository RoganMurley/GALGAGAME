module Stack.View exposing (view)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), CardLimbo(..))
import Array
import Card.View as Card
import Game.Types exposing (Context, StackEntity)
import WebGL
import WhichPlayer.State exposing (other)


view : List StackEntity -> Context -> List WebGL.Entity
view entities ctx =
    let
        n =
            List.length entities
                - 1

        makeEntity i =
            case ctx.anim of
                Reflect _ ->
                    if i == n then
                        Card.view ctx

                    else
                        \entity ->
                            let
                                ca =
                                    { owner = other entity.owner
                                    , card = entity.card
                                    }

                                cb =
                                    { owner = entity.owner
                                    , card = entity.card
                                    }
                            in
                            Card.transmutingView ctx ca cb entity

                Transmute _ ca cb _ ->
                    if i == 0 then
                        Card.transmutingView ctx ca cb

                    else
                        Card.view ctx

                Fabricate _ ->
                    if i == 0 then
                        Card.fabricatingView ctx

                    else
                        Card.view ctx

                Bounce bounces ->
                    case Array.get i <| Array.fromList bounces of
                        Just (BounceIndex _ _) ->
                            \_ -> []

                        Just BounceDiscard ->
                            Card.dissolvingView ctx

                        _ ->
                            Card.view ctx

                Discard discards ->
                    case Array.get i <| Array.fromList discards of
                        Just CardDiscard ->
                            Card.dissolvingView ctx

                        _ ->
                            Card.view ctx

                Limbo limbos ->
                    case Array.get i <| Array.fromList limbos of
                        Just CardLimbo ->
                            Card.limboingView ctx

                        _ ->
                            Card.view ctx

                Unlimbo _ ->
                    Card.limboingView ctx

                _ ->
                    Card.view ctx
    in
    List.concat <| List.indexedMap makeEntity entities
