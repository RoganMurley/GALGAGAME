module Connected.View exposing (view)

import Connected.Types exposing (..)


view : Model -> Html Msg
view model =
    div [] [ text "hello, world!" ]
