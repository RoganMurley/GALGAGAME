module Routing.State exposing (loginRoute, playRoute, replayRoute, route, specRoute)

import Routing.Types exposing (PlayRoute(..), Route(..))
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
        , map Spec specRoute
        , map Replay replayRoute
        , map Login loginRoute
        , map Signup signupRoute
        , map Home top
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
