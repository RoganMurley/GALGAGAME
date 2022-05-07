module Leaderboard.View exposing (view)

import Html exposing (Html, a, div, table, td, text, th, tr)
import Html.Attributes exposing (class, href)
import Leaderboard.Types exposing (Entry, Model)
import Util exposing (maybeListToListMaybe)


view : Model -> Bool -> Html a
view { entries } detailed =
    let
        entryView : Int -> Maybe Entry -> Html a
        entryView i mEntry =
            let
                usernameTag =
                    if detailed then
                        a

                    else
                        div

                { usernameHtml, levelHtml, xpHtml } =
                    case mEntry of
                        Just { name, xp, level } ->
                            let
                                profileUrl =
                                    "/profile/" ++ name
                            in
                            { usernameHtml = usernameTag [ class "username", href profileUrl ] [ text name ]
                            , levelHtml = text <| String.fromInt level
                            , xpHtml = text <| String.fromInt (floor xp)
                            }

                        Nothing ->
                            { usernameHtml = usernameTag [ class "username" ] []
                            , levelHtml = text ""
                            , xpHtml = text ""
                            }
            in
            tr []
                [ td [ class "leaderboard-rank" ] [ text <| String.fromInt (i + 1) ]
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
                :: List.indexedMap entryView (maybeListToListMaybe 10 entries)
            )
        ]
