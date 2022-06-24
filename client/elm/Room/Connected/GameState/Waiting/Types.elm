module Waiting.Types exposing (Model, WaitType(..))

import Random exposing (Seed)


type alias Model =
    { waitType : WaitType
    , bounceTick : Float
    , seed : Maybe Seed
    }


type WaitType
    = WaitQuickplay
    | WaitCustom
