module Card.Types exposing (Card, Entity)

import Game.Entity as Game
import WhichPlayer.Types exposing (WhichPlayer)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    }


type alias Entity a =
    Game.Entity
        { a
            | card : Card
            , owner : WhichPlayer
        }
