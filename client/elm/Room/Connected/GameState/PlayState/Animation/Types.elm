module Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), HandBounce, Hurt(..), KnowableCard(..), Transmutation(..))

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3)
import Stack.Types exposing (StackCard)
import Wheel.Types exposing (Wheel)
import WhichPlayer.Types exposing (WhichPlayer)


type Anim
    = NullAnim
    | Hurt WhichPlayer Int Hurt
    | Heal WhichPlayer Int
    | Draw WhichPlayer Float
    | Play WhichPlayer KnowableCard Int (Maybe Vec3)
    | Transmute (Wheel (Maybe Transmutation))
    | Mill WhichPlayer Card Float
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Windup WhichPlayer
    | Bounce (Wheel (Maybe Bounce))
    | DiscardStack (Wheel Bool)
    | DiscardHand WhichPlayer (List CardDiscard)
    | MoveStack (Wheel (Maybe Int)) Int
    | Pass WhichPlayer
    | HandFullPass
    | Finding
    | GetGen
    | Timeout


type Hurt
    = Slash
    | Bite
    | Curse


type Bounce
    = BounceDiscard
    | BounceIndex Int Int


type CardDiscard
    = NoDiscard Int
    | CardDiscard


type Transmutation
    = Transmutation StackCard StackCard


type KnowableCard
    = KnownCard Card
    | UnknownCard Card


type alias HandBounce =
    { handIndex : Int
    , stackIndex : Int
    , card : Card
    }
