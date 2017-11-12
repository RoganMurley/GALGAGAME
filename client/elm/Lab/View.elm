module Lab.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lab.Messages exposing (Msg(..))
import Lab.Types exposing (Model)
import Raymarch.Types exposing (Params(..))
import Animation.View as Animation


view : Params -> Model -> Html Msg
view params { player, anim, time } =
    Animation.view params time (Just ( player, anim ))
