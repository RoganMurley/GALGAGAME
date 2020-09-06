module Holding.View exposing (view)

import Card.State as Card
import Card.View as Card
import Game.Types exposing (Context)
import Holding.Types exposing (Holding(..))
import Quaternion
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Holding -> Context -> List WebGL.Entity
view holding ctx =
    case holding of
        NoHolding ->
            []

        Holding card _ pos ->
            Card.view ctx
                { position = pos
                , rotation = Quaternion.identity
                , scale = Card.scale
                , card = card
                , owner = PlayerA
                }
