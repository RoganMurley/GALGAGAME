module Profile.Messages exposing (..)

import Http
import Profile.Types exposing (Profile)


type Msg
    = Load String
    | LoadCallback (Result Http.Error Profile)
