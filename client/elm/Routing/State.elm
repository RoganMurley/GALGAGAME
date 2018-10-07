module Routing.State exposing (..)

import Routing.Types exposing (PlayRoute(..), Route(..))
import UrlParser exposing (Parser, (</>), s, string, top, map, oneOf)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
        , map Spec specRoute
        , map Replay replayRoute
        , map Login loginRoute
        , map Home top
        ]


playRoute : Parser (PlayRoute -> a) a
playRoute =
    s "play"
        </> oneOf
                [ map ComputerPlay (s "computer")
                , map (CustomPlay << Just) (s "custom" </> string)
                , map (CustomPlay Nothing) (s "custom")
                , map QuickPlay (s "quickplay")
                ]


specRoute : Parser (String -> a) a
specRoute =
    s "spec" </> string


loginRoute : Parser a a
loginRoute =
    s "login"


replayRoute : Parser (String -> a) a
replayRoute =
    s "replay" </> string
