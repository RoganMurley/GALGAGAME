module Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), CardLimbo(..), HandBounce, Hurt(..), Transmutation(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)
import Stack.Types exposing (StackCard)
import WhichPlayer.Types exposing (WhichPlayer)


type Anim
    = NullAnim
    | Hurt WhichPlayer Int Hurt
    | Heal WhichPlayer Int
    | Draw WhichPlayer
    | Reverse WhichPlayer
    | Confound WhichPlayer
    | Play WhichPlayer Card Int (Maybe Vec3)
    | Transmute (List Transmutation)
    | Mill WhichPlayer Card
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Windup WhichPlayer
    | Fabricate StackCard
    | Bounce (List Bounce)
    | DiscardStack (List CardDiscard)
    | DiscardHand WhichPlayer (List CardDiscard)
    | Pass WhichPlayer
    | Limbo (List CardLimbo)
    | Unlimbo WhichPlayer
    | HandFullPass
    | Finding


type Hurt
    = Slash
    | Bite
    | Curse


type Bounce
    = NoBounce Int
    | BounceDiscard
    | BounceIndex Int Int


type CardDiscard
    = NoDiscard Int
    | CardDiscard


type CardLimbo
    = NoLimbo Int
    | CardLimbo


type Transmutation
    = Transmutation StackCard StackCard
    | NoTransmutation


type alias HandBounce =
    { handIndex : Int
    , stackIndex : Int
    , card : Card
    }
