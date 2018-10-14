module Animation.Types exposing (..)

import Card.Types exposing (Card)
import Stack.Types exposing (StackCard)
import WhichPlayer.Types exposing (WhichPlayer)


type Anim
    = NullAnim
    | Slash WhichPlayer Int
    | Heal WhichPlayer
    | Draw WhichPlayer
    | Bite WhichPlayer Int
    | Reflect WhichPlayer
    | Reverse WhichPlayer
    | Obliterate WhichPlayer
    | Play WhichPlayer Card Int
    | Transmute WhichPlayer StackCard StackCard
    | Mill WhichPlayer Card
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Windup WhichPlayer
