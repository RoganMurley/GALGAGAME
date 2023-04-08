module Profile.View exposing (view)

import Html exposing (Html, a, div, h1, h2, span, table, td, text, tr)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Main.Messages exposing (Msg(..))
import Profile.Types exposing (Model, ProfileReplay)
import Util exposing (maybeListToListMaybe)


view : Model -> Html Msg
view model =
    div [ class "profile-box" ] <|
        case model.error of
            "" ->
                let
                    { nameHtml, onlineHtml, levelHtml, replaysHtmlList, challengeHtml } =
                        case model.profile of
                            Just { name, level, xp, replays, online, id, isMe } ->
                                { nameHtml = text name
                                , levelHtml =
                                    text <|
                                        "Level "
                                            ++ String.fromInt level
                                            ++ " / "
                                            ++ String.fromFloat xp
                                            ++ " XP"
                                , replaysHtmlList = List.map profileReplayView <| maybeListToListMaybe 10 (Just replays)
                                , onlineHtml =
                                    if online then
                                        span [ class "online-dot" ] []

                                    else
                                        span [] []
                                , challengeHtml =
                                    if online && not isMe then
                                        a [ href "#", class "button challenge", onClick <| Challenge id ] [ text "CHALLENGE" ]

                                    else
                                        text ""
                                }

                            Nothing ->
                                { nameHtml = text ""
                                , levelHtml = text ""
                                , replaysHtmlList = List.map profileReplayView <| maybeListToListMaybe 10 Nothing
                                , onlineHtml = text ""
                                , challengeHtml = text ""
                                }
                in
                [ div [ class "profile" ] <|
                    [ h1 [ class "username" ] [ onlineHtml, nameHtml ]
                    , h2 [ class "level" ] [ levelHtml ]
                    , challengeHtml
                    , table [ class "replays" ] replaysHtmlList
                    , div [ class "view-leaderboard-spacer" ] []
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
            tr []
                [ td [ class "replay" ]
                    [ a
                        [ href <| "/replay/" ++ String.fromInt id ]
                        [ span [ class "username" ] [ text usernamePa ]
                        , text " vs "
                        , span [ class "username" ] [ text usernamePb ]
                        ]
                    ]
                ]

        Nothing ->
            tr [] [ td [ class "replay" ] [] ]
