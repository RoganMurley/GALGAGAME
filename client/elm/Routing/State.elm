module Routing.State exposing (default, loginRoute, playRoute, replayRoute, route, specRoute)

import Routing.Types exposing (PlayRoute(..), Route(..))
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)


default : Route
default =
    Play WorldPlay


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
        , map Spec specRoute
        , map Replay replayRoute
        , map Login loginRoute
        , map Signup signupRoute
        , map Feedback feedbackRoute
        ]


playRoute : Parser (PlayRoute -> a) a
playRoute =
    s "play"
        </> oneOf
                [ map ComputerPlay <| s "computer"
                , map (CustomPlay << Just) <| s "custom" </> string
                , map (CustomPlay Nothing) <| s "custom"
                , map QuickPlay <| s "quickplay"
                , map TutorialPlay <| s "tutorial"
                , map DailyPlay <| s "daily"
                , map WorldPlay <| s "journey"
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


replayRoute : Parser (String -> a) a
replayRoute =
    s "replay" </> string


feedbackRoute : Parser a a
feedbackRoute =
    s "feedback"
