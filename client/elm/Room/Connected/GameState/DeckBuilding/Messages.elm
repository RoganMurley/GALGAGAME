module DeckBuilding.Messages exposing (Msg(..))

import DeckBuilding.Types exposing (Character)


type Msg
    = Select Character
    | NextCharacter
    | PreviousCharacter
