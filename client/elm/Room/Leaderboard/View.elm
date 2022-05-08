module Leaderboard.View exposing (view)

import Html exposing (Html, a, div, table, td, text, th, tr)
import Html.Attributes exposing (class, href)
import Leaderboard.Types exposing (Entry, Model)
import Util exposing (maybeListToListMaybe)


view : Model -> Bool -> Html a
view { entries } detailed =
    let
        entryView : Maybe Entry -> Html a
        entryView mEntry =
            let
                usernameTag =
                    if detailed then
                        a

                    else
                        div

                { usernameHtml, levelHtml, xpHtml, rankHtml, isMeClass } =
                    case mEntry of
                        Just { name, xp, level, rank, isMe } ->
                            let
                                profileUrl =
                                    "/profile/" ++ name
                            in
                            { usernameHtml = usernameTag [ class "username", href profileUrl ] [ text name ]
                            , levelHtml = text <| String.fromInt level
                            , xpHtml = text <| String.fromInt (floor xp)
                            , rankHtml = text <| String.fromInt rank
                            , isMeClass =
                                if isMe then
                                    "leaderboard-me"

                                else
                                    ""
                            }

                        Nothing ->
                            { rankHtml = text ""
                            , usernameHtml = usernameTag [ class "username" ] []
                            , levelHtml = text ""
                            , xpHtml = text ""
                            , isMeClass = ""
                            }
            in
            tr [ class isMeClass ]
                [ td [ class "leaderboard-rank" ] [ rankHtml ]
                , td [ class "leaderboard-name" ] [ usernameHtml ]
                , td [] [ levelHtml ]
                , td [] [ xpHtml ]
                ]
    in
    div [ class "leaderboard-box" ]
        [ table []
            (tr []
                [ th [ class "leaderboard-rank" ] [ text "Rank" ]
                , th [ class "leaderboard-name" ] [ text "Username" ]
                , th [] [ text "Level" ]
                , th [] [ text "XP" ]
                ]
                :: List.map entryView (maybeListToListMaybe 10 entries)
            )
        ]
