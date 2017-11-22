module CharacterSelect.Types exposing (..)

import CharacterSelect.Character exposing (Character)
import CharacterSelect.ViewModel exposing (ViewModel)


type alias Model =
    { characters : List Character
    , selected : List Character
    , vm : ViewModel
    }
