module Lab.Messages exposing (Msg(..))

import Animation.Types exposing (Anim)
import Model.Types exposing (WhichPlayer)


type Msg
    = SetPlayer WhichPlayer
    | SetAnim Anim
