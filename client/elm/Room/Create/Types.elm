module Create.Types exposing (Field(..), Model)

import Form exposing (FormField)


type alias Model =
    { allowSpectators : FormField
    , startingLife : FormField
    , error : String
    , submitting : Bool
    }


type Field
    = AllowSpectators
    | StartingLife
