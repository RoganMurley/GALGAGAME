module CharacterSelect.ViewModel exposing (..)

import CharacterSelect.Character exposing (Character)


type alias ViewModel =
    { hover : Character
    }


init : Character -> ViewModel
init character =
    { hover = character
    }
