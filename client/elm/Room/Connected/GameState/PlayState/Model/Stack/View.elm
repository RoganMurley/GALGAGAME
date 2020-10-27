module Stack.View exposing (view)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), CardLimbo(..), Transmutation(..))
import Array
import Card.View as Card
import Game.Types exposing (Context, StackEntity)
import WebGL


view : List StackEntity -> Context -> List WebGL.Entity
view entities ctx =
    let
        makeEntity i =
            case ctx.anim of
                Transmute transmutations ->
                    case Array.get i <| Array.fromList transmutations of
                        Just (Transmutation ca cb) ->
                            Card.transmutingView ctx ca cb

                        _ ->
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

                DiscardStack discards ->
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
