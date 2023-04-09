module Waiting.Types exposing (Model, WaitTime, WaitType(..))

import Random exposing (Seed)


type alias Model =
    { waitType : Maybe WaitType
    , bounceTick : Float
    , seed : Maybe Seed
    , bulge : Float
    }


type WaitType
    = WaitQuickplay
    | WaitCustom
    | WaitChallenge WaitTime


type alias WaitTime =
    Float
