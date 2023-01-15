module Create.Types exposing (Field(..), Model)

import Form exposing (FormField)


type alias Model =
    { allowSpectators : Bool
    , startingLife : FormField
    , error : String
    }


type Field
    = StartingLife
