module Audio.Messages exposing (Msg(..))

import Dict exposing (Dict)


type Msg
    = SoundsLoaded (Dict String String)
