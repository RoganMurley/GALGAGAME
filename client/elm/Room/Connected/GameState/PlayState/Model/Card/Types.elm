module Card.Types exposing (Card, Entity)

import Game.Entity as Game
import Status.Types exposing (Status)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    , statuses : List Status
    }


type alias Entity a =
    Game.Entity3D
        { a
            | card : Card
            , owner : WhichPlayer
        }
