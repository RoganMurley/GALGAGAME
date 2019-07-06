module Animation.Types exposing (Anim(..), Bounce(..), HandBounce, Hurt(..), Transmute(..))

import Card.Types exposing (Card)
import Stack.Types exposing (StackCard)
import WhichPlayer.Types exposing (WhichPlayer)


type Anim
    = NullAnim
    | Hurt WhichPlayer Int Hurt
    | Heal WhichPlayer Int
    | Draw WhichPlayer
    | Reflect WhichPlayer
    | Reverse WhichPlayer
    | Hubris WhichPlayer
    | Confound WhichPlayer
    | Play WhichPlayer Card Int
    | Transmute WhichPlayer StackCard StackCard Transmute
    | Mill WhichPlayer Card
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Windup WhichPlayer
    | Fabricate StackCard
    | Bounce (List Bounce)
    | Pass WhichPlayer
    | HandFullPass
    | Infinity WhichPlayer
    | Finding


type Hurt
    = Slash
    | Bite
    | Curse


type Transmute
    = TransmuteCard
    | TransmuteOwner


type Bounce
    = NoBounce Int
    | BounceDiscard
    | BounceIndex Int Int


type alias HandBounce =
    { handIndex : Int
    , stackIndex : Int
    , card : Card
    }
