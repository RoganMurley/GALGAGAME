module CharacterSelect.Messages exposing (Msg(..))

import CharacterSelect.Character exposing (Character)


type Msg
    = Hover Character
    | Select Character
