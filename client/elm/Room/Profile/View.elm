module Profile.View exposing (view)

import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class)
import Profile.Types exposing (Model)


view : Model -> Html a
view { profile, error } =
    div [ class "profile-box" ] <|
        case profile of
            Just { name, level, xp } ->
                [ div [ class "profile" ]
                    [ h1 [ class "username" ] [ text name ]
                    , h2 []
                        [ text <|
                            "Level "
                                ++ String.fromInt level
                                ++ " / "
                                ++ String.fromFloat xp
                                ++ " XP"
                        ]
                    ]
                ]

            Nothing ->
                [ div [ class "error" ] [ text error ] ]
