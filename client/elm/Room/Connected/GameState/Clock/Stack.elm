module Clock.Stack exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Card exposing (CardEntity, cardEntity, dissolvingCardEntity, transmutingCardEntity)
import Clock.Types exposing (Context)
import WebGL
import WhichPlayer.State exposing (other)


view : List (CardEntity {}) -> Context -> List WebGL.Entity
view entities ctx =
    let
        n =
            List.length entities - 1

        makeEntity i =
            case ctx.anim of
                Obliterate _ ->
                    if i == n then
                        cardEntity ctx
                    else
                        dissolvingCardEntity ctx

                Reflect _ ->
                    if i == n then
                        cardEntity ctx
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
                                transmutingCardEntity
                                    ctx
                                    ca
                                    cb
                                    entity

                Transmute _ ca cb ->
                    if i == 0 then
                        transmutingCardEntity ctx ca cb
                    else
                        cardEntity ctx

                _ ->
                    cardEntity ctx
    in
        List.concat <| List.indexedMap makeEntity entities
