module Resolvable.Types exposing (..)

import Animation.Types exposing (Anim)
import Model.Types as Model
import Model.ViewModel exposing (ViewModel)


type alias Model =
    { vm : ViewModel
    , tick : Float
    , final : Model.Model
    , resList : List ResolveData
    }


type alias ResolveData =
    { model : Model.Model
    , anim : Maybe Anim
    , stackCard : Model.StackCard
    }
