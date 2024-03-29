module Card.Types exposing (Card, Entity, KnowableCard(..), Related)

import Game.Entity as Game
import Status.Types exposing (Status)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    , statuses : List Status
    , related : List Related
    }


type alias Related =
    { name : String
    , desc : String
    , imgURL : String
    }


type KnowableCard
    = KnownCard Card
    | UnknownCard Card


type alias Entity a =
    Game.Entity3D
        { a
            | card : Card
            , owner : WhichPlayer
            , revealed : Bool
        }
