module RuneSelect.Entities exposing (entities)

import Card.State as Card
import Card.Types as Card
import Game.Types exposing (Context)
import Math.Vector3 exposing (vec3)
import Quaternion
import RuneSelect.Types exposing (Model)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : Context -> Model -> List (Card.Entity {})
entities _ model =
    let
        rune =
            model.carousel.selected

        rotation =
            Quaternion.identity
    in
    [ { position = vec3 0 -0.7 0
      , rotation = rotation
      , scale = Card.scale
      , card = rune.cards.b
      , owner = PlayerA
      }
    , { position = vec3 0.7 0 0
      , rotation = rotation
      , scale = Card.scale
      , card = rune.cards.a
      , owner = PlayerA
      }
    , { position = vec3 0 0.7 0
      , rotation = rotation
      , scale = Card.scale
      , card = rune.cards.d
      , owner = PlayerA
      }
    , { position = vec3 -0.7 0 0
      , rotation = rotation
      , scale = Card.scale
      , card = rune.cards.c
      , owner = PlayerA
      }
    ]
