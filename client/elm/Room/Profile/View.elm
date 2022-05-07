module Profile.View exposing (view)

import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class, href)
import Profile.Types exposing (Model, ProfileReplay)
import Util exposing (maybeListToListMaybe)


view : Model -> Html a
view model =
    div [ class "profile-box" ] <|
        case model.error of
            "" ->
                let
                    { nameHtml, levelHtml, replaysHtmlList } =
                        case model.profile of
                            Just { name, level, xp, replays } ->
                                { nameHtml = text name
                                , levelHtml =
                                    text <|
                                        "Level "
                                            ++ String.fromInt level
                                            ++ " / "
                                            ++ String.fromFloat xp
                                            ++ " XP"
                                , replaysHtmlList = List.map profileReplayView <| maybeListToListMaybe 10 (Just replays)
                                }

                            Nothing ->
                                { nameHtml = text ""
                                , levelHtml = text ""
                                , replaysHtmlList = List.map profileReplayView <| maybeListToListMaybe 10 Nothing
                                }
                in
                [ div [ class "profile" ] <|
                    [ h1 [ class "username" ] [ nameHtml ]
                    , h2 [ class "level" ] [ levelHtml ]
                    , div [ class "replays" ] replaysHtmlList
                    , a [ class "button", href "/leaderboard" ] [ text "VIEW LEADERBOARD" ]
                    ]
                ]

            _ ->
                [ div [ class "error" ] [ text model.error ] ]


profileReplayView : Maybe ProfileReplay -> Html a
profileReplayView mReplay =
    case mReplay of
        Just { pa, pb, id } ->
            let
                usernamePa =
                    Maybe.withDefault "???" pa

                usernamePb =
                    Maybe.withDefault "???" pb
            in
            div [ class "replay" ]
                [ a
                    [ href <| "/replay/" ++ String.fromInt id ]
                    [ span [ class "username" ] [ text usernamePa ]
                    , text " vs "
                    , span [ class "username" ] [ text usernamePb ]
                    ]
                ]

        Nothing ->
            div [ class "replay" ] []
