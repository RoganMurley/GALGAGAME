module Routing.State exposing (default, loginRoute, playRoute, replayRoute, route, specRoute)

import Routing.Types exposing (PlayRoute(..), Route(..))
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string)
import Url.Parser.Query as Query


default : Route
default =
    Play <| QuickPlay Nothing


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
        , map Spec specRoute
        , map Replay replayRoute
        , map Login loginRoute
        , map Signup signupRoute
        , map Feedback feedbackRoute
        , map League leagueRoute
        , map Leaderboard leaderboardRoute
        , map Profile profileRoute
        , map Presence presenceRoute
        , map Create createRoute
        ]


playRoute : Parser (PlayRoute -> a) a
playRoute =
    s "play"
        </> oneOf
                [ map (ComputerPlay << Just) <| s "computer" </> string
                , map (ComputerPlay Nothing) <| s "computer"
                , map (CustomPlay << Just) <| s "custom" </> string
                , map (CustomPlay Nothing) <| s "custom"
                , map (QuickPlay << Just) <| s "quickplay" </> string
                , map (QuickPlay Nothing) <| s "quickplay"
                , map (ChallengePlay << Just) <| s "challenge" </> string
                , map (ChallengePlay Nothing) <| s "challenge"
                ]


specRoute : Parser (String -> a) a
specRoute =
    s "spec" </> string


loginRoute : Parser a a
loginRoute =
    s "login"


signupRoute : Parser a a
signupRoute =
    s "signup"


replayRoute : Parser (String -> Maybe Int -> a) a
replayRoute =
    s "replay" </> string <?> Query.int "t"


feedbackRoute : Parser a a
feedbackRoute =
    s "feedback"


leagueRoute : Parser a a
leagueRoute =
    s "league"


leaderboardRoute : Parser a a
leaderboardRoute =
    s "leaderboard"


presenceRoute : Parser a a
presenceRoute =
    s "presence"


profileRoute : Parser (String -> a) a
profileRoute =
    s "profile" </> string


createRoute : Parser a a
createRoute =
    s "create"
