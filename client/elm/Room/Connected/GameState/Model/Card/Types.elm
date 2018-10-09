module Card.Types exposing (Card, Entity)

import Clock.Entity exposing (GameEntity)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    }


type alias Entity a =
    GameEntity
        { a
            | card : Card
            , owner : WhichPlayer
        }
