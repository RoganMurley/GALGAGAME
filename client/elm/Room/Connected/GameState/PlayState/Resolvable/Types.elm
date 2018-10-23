module Resolvable.Types exposing (Model, ResolveData, ResolveDiffData)

import Animation.Types exposing (Anim)
import Model.Diff as Model
import Model.Types as Model
import Model.ViewModel exposing (ViewModel)
import Stack.Types exposing (StackCard)


type alias Model =
    { vm : ViewModel
    , tick : Float
    , final : Model.Model
    , resList : List ResolveData
    }


type alias ResolveData =
    { model : Model.Model
    , anim : Anim
    , stackCard : Maybe StackCard
    }


type alias ResolveDiffData =
    { diff : Model.Diff
    , anim : Anim
    , stackCard : Maybe StackCard
    }
