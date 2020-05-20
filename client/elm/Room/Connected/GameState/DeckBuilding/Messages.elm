module DeckBuilding.Messages exposing (Msg(..))

import DeckBuilding.Types exposing (Character, Rune, RuneCursor)


type Msg
    = Select Character
    | NextCharacter
    | PreviousCharacter
    | EnterRuneSelect RuneCursor
    | SelectRune Rune
