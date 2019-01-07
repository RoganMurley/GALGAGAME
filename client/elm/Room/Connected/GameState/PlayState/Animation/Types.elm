module Animation.Types exposing (Anim(..), Bounce(..), HandBounce)

import Card.Types exposing (Card)
import Stack.Types exposing (StackCard)
import WhichPlayer.Types exposing (WhichPlayer)


type Anim
    = NullAnim
    | Slash WhichPlayer Int
    | Heal WhichPlayer Int
    | Draw WhichPlayer
    | Bite WhichPlayer Int
    | Reflect WhichPlayer
    | Reverse WhichPlayer
    | Hubris WhichPlayer
    | Play WhichPlayer Card Int
    | Transmute WhichPlayer StackCard StackCard
    | Mill WhichPlayer Card
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Windup WhichPlayer
    | Fabricate StackCard
    | Bounce (List Bounce)


type Bounce
    = NoBounce Int
    | BounceDiscard
    | BounceIndex Int Int


type alias HandBounce =
    { handIndex : Int
    , stackIndex : Int
    , card : Card
    }
