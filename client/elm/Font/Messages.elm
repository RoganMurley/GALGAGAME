module Font.Messages exposing (Msg(..))

import Font.Types exposing (Font)


type Msg
    = FontLoaded ( String, Font )
    | FontError String
