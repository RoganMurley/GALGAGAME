module Lab.Messages exposing (Msg(..))

import Card.Types exposing (Anim)
import Model.Types exposing (WhichPlayer)


type Msg
    = SetPlayer WhichPlayer
    | SetAnim Anim
