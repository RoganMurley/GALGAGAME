module RuneSelect.Entities exposing (entities)

import Card.Types as Card
import Game.Types exposing (Context)
import Math.Vector2 exposing (vec2)
import RuneSelect.Types exposing (Model)
import WhichPlayer.Types exposing (WhichPlayer(..))


entities : Context -> Model -> List (Card.Entity {})
entities { radius, w, h } model =
    let
        rune =
            model.carousel.selected
    in
    [ { position = vec2 (w * 0.5) (h * 0.5 - radius * 0.7)
      , rotation = pi
      , scale = 1.4
      , card = rune.cards.b
      , owner = PlayerA
      }
    , { position = vec2 (w * 0.5 + radius * 0.7) (h * 0.5)
      , rotation = pi
      , scale = 1.4
      , card = rune.cards.a
      , owner = PlayerA
      }
    , { position = vec2 (w * 0.5) (h * 0.5 + radius * 0.7)
      , rotation = pi
      , scale = 1.4
      , card = rune.cards.d
      , owner = PlayerA
      }
    , { position = vec2 (w * 0.5 - radius * 0.7) (h * 0.5)
      , rotation = pi
      , scale = 1.4
      , card = rune.cards.c
      , owner = PlayerA
      }
    ]
