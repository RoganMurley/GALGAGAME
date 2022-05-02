module Profile.View exposing (view)

import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class, href)
import Profile.Types exposing (Model, ProfileReplay)


view : Model -> Html a
view { profile, error } =
    div [ class "profile-box" ] <|
        case profile of
            Just { name, level, xp, replays } ->
                [ div [ class "profile" ] <|
                    [ h1 [ class "username" ] [ text name ]
                    , h2 []
                        [ text <|
                            "Level "
                                ++ String.fromInt level
                                ++ " / "
                                ++ String.fromFloat xp
                                ++ " XP"
                        ]
                    , div [ class "replays" ] <| List.map profileReplayView replays
                    , a [ class "button", href "/leaderboard" ] [ text "VIEW LEADERBOARD" ]
                    ]
                ]

            Nothing ->
                [ div [ class "error" ] [ text error ] ]


profileReplayView : ProfileReplay -> Html a
profileReplayView { pa, pb, id } =
    let
        usernamePa =
            Maybe.withDefault "???" pa

        usernamePb =
            Maybe.withDefault "???" pb
    in
    div []
        [ a
            [ href <| "/replay/" ++ String.fromInt id ]
            [ span [ class "username" ] [ text usernamePa ]
            , text " vs "
            , span [ class "username" ] [ text usernamePb ]
            ]
        ]
