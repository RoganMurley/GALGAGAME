module Stack.View exposing (..)

import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Card.View as Card
import Clock.Types exposing (Context)
import WebGL
import WhichPlayer.State exposing (other)


view : List (Card.Entity {}) -> Context -> List WebGL.Entity
view entities ctx =
    let
        n =
            List.length entities - 1

        makeEntity i =
            case ctx.anim of
                Obliterate _ ->
                    if i == n then
                        Card.view ctx
                    else
                        Card.dissolvingView ctx

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

                Transmute _ ca cb ->
                    if i == 0 then
                        Card.transmutingView ctx ca cb
                    else
                        Card.view ctx

                _ ->
                    Card.view ctx
    in
        List.concat <| List.indexedMap makeEntity entities
