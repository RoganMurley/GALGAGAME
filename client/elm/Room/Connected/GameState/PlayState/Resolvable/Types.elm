module Resolvable.Types exposing (Model, ResolveData, ResolveDiffData)

import Animation.Types exposing (Anim)
import Model.Diff as Model
import Model.Types as Model


type alias Model =
    { tick : Float
    , final : Model.Model
    , resList : List ResolveData
    }


type alias ResolveData =
    { model : Model.Model
    , anim : Anim
    , animDamage : ( Float, Float )
    }


type alias ResolveDiffData =
    { diff : Model.Diff
    , anim : Anim
    , animDamage : ( Float, Float )
    }
