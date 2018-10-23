module Model.ViewModel exposing (ViewModel, init)


type alias ViewModel =
    { hover : Maybe Int }


init : ViewModel
init =
    { hover = Nothing }
