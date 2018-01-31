module Lab.Messages exposing (Msg(..))

import Animation.Types exposing (Anim)
import WhichPlayer.Types exposing (WhichPlayer)


type Msg
    = SetPlayer WhichPlayer
    | SetAnim Anim
