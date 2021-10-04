module Waiting.Types exposing (Model, WaitType(..))


type alias Model =
    { waitType : WaitType
    , bounceTick : Float
    }


type WaitType
    = WaitQuickplay
    | WaitCustom
