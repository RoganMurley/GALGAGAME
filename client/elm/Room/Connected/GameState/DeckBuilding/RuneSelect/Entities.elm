module RuneSelect.Entities exposing (entities)

import Card.Types as Card
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate)
import Math.Vector3 exposing (vec3)
import Quaternion exposing (Quaternion)
import RuneSelect.Types exposing (Model)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : Context -> Model -> List (Card.Entity {})
entities { radius, w, h } model =
    let
        rune =
            model.carousel.selected

        rotation =
            Quaternion.identity
    in
    [ { position = vec3 0 -0.7 0
      , rotation = rotation
      , scale = 1.4
      , card = rune.cards.b
      , owner = PlayerA
      }
    , { position = vec3 0.7 0 0
      , rotation = rotation
      , scale = 1.4
      , card = rune.cards.a
      , owner = PlayerA
      }
    , { position = vec3 0 0.7 0
      , rotation = rotation
      , scale = 1.4
      , card = rune.cards.d
      , owner = PlayerA
      }
    , { position = vec3 -0.7 0 0
      , rotation = rotation
      , scale = 1.4
      , card = rune.cards.c
      , owner = PlayerA
      }
    ]
