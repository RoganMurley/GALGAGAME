module DeckBuilding.Messages exposing (Msg(..))

import DeckBuilding.Types exposing (Character)
import RuneSelect.Messages as RuneSelect
import RuneSelect.Types exposing (Rune, RuneCursor)


type Msg
    = Select Character
    | ConfirmRune RuneCursor Rune
    | EnterRuneSelect RuneCursor
    | RuneSelectMsg RuneSelect.Msg
