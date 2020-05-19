module DeckBuilding.Messages exposing (Msg(..))

import DeckBuilding.Types exposing (Character, Rune)


type Msg
    = Select Character
    | NextCharacter
    | PreviousCharacter
    | EnterRuneSelect Rune ExcludeRune ExcludeRune Int
    | UpdateRune Rune Int


type alias ExcludeRune =
    Rune
