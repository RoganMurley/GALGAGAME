module Clock.Stack exposing (..)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(..))
import Clock.Card exposing (CardEntity, cardEntity, dissolvingCardEntity, transmutingCardEntity)
import Clock.Types exposing (ClockParams, GameEntity)
import Ease
import Maybe.Extra as Maybe
import Texture.Types as Texture
import WebGL
import WhichPlayer.State exposing (other)


view : ClockParams -> List (CardEntity {}) -> Maybe ( Float, Maybe Anim ) -> Texture.Model -> List WebGL.Entity
view params entities resInfo textures =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        maxTick =
            animToResTickMax anim

        progress =
            Ease.outQuint <| resTick / maxTick

        n =
            List.length entities - 1

        makeEntity i =
            case anim of
                Just (Obliterate _) ->
                    if i == n then
                        cardEntity params textures
                    else
                        dissolvingCardEntity params textures progress

                Just (Reflect _) ->
                    if i == n then
                        cardEntity params textures
                    else
                        (\entity ->
                            let
                                changeOwner =
                                    identity

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
                                    params
                                    textures
                                    progress
                                    ca
                                    cb
                                    entity
                        )

                Just (Transmute _ ca cb) ->
                    if i == 0 then
                        transmutingCardEntity params textures progress ca cb
                    else
                        cardEntity params textures

                _ ->
                    cardEntity params textures
    in
        List.concat <| List.indexedMap makeEntity entities
