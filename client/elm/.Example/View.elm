module Example.View exposing (view)

import Example.Types exposing (Model)


view : Model -> Html Msg
view model =
    div [] [ text "hello, world!" ]
