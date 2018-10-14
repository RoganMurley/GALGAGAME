module Model.ViewModel exposing (..)


type alias ViewModel =
    { hover : Maybe Int }


init : ViewModel
init =
    { hover = Nothing }
