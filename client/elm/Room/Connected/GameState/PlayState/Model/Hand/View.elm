module Hand.View exposing (millView, otherView, view)

import Animation.State as Animation
import Animation.Types exposing (Anim(..), CardDiscard(..))
import Array
import Card.State as Card
import Card.Types as Card
import Card.View as Card
import Ease
import Game.Types exposing (Context, HandEntity, OtherHandEntity)
import Hand.Entities exposing (applyHoverVector)
import Math.Vector3 exposing (vec3)
import Quaternion
import Util exposing (interp)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : List HandEntity -> Context -> List WebGL.Entity
view handEntities ctx =
    let
        cardView handEntity =
            let
                entity =
                    applyHoverVector handEntity
            in
            if handEntity.discarding then
                List.concat
                    [ Card.dissolvingView ctx entity ]

            else
                List.concat
                    [ Card.view ctx entity, Card.revealedView ctx entity ]
    in
    List.concat <|
        List.map cardView handEntities


otherView : List OtherHandEntity -> Context -> List WebGL.Entity
otherView otherHandEntities ctx =
    let
        -- Make DRY with hand view
        cardView i entity =
            case entity.mCard of
                Just card ->
                    let
                        knownEntity =
                            { position = entity.position
                            , scale = entity.scale
                            , rotation = entity.rotation
                            , card = card
                            , owner = PlayerB
                            , revealed = False
                            , discarding = False
                            }
                    in
                    case ctx.anim of
                        Reveal PlayerB reveal ->
                            case Array.get i <| Array.fromList reveal of
                                Just True ->
                                    let
                                        startCard =
                                            { owner = PlayerB
                                            , card =
                                                { name = ""
                                                , desc = ""
                                                , imgURL = "invisible.png"
                                                , statuses = []
                                                }
                                            }

                                        endCard =
                                            { owner = PlayerB
                                            , card = card
                                            }
                                    in
                                    Card.transmutingView ctx startCard endCard knownEntity

                                _ ->
                                    Card.view ctx knownEntity

                        _ ->
                            if entity.discarding then
                                Card.dissolvingView ctx { knownEntity | discarding = True }

                            else
                                List.concat
                                    [ Card.view ctx knownEntity, Card.revealedView ctx knownEntity ]

                Nothing ->
                    if entity.discarding then
                        Card.backDissolvingView ctx entity

                    else
                        Card.backView ctx entity
    in
    List.concat <|
        List.indexedMap cardView otherHandEntities


millView : Context -> List WebGL.Entity
millView ({ progress, tick, anim } as ctx) =
    case anim of
        Mill owner card _ ->
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
                            vec3 -1 0 0

                        PlayerB ->
                            vec3 -1 1 0

                entity =
                    { owner = owner
                    , card = card
                    , position =
                        interp
                            progress
                            startPos
                            (vec3 0 0 -1)
                    , rotation =
                        Quaternion.lerp
                            progress
                            Quaternion.identity
                            (Quaternion.zRotation (-sign * 0.05 * pi))
                    , scale = Card.scale
                    , revealed = False
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
