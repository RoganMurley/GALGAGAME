module Routing.State exposing (..)

import Routing.Types exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, top, map, oneOf)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
        , map Spec specRoute
        , map Lab labRoute
        , map Home top
        ]


playRoute : Parser (PlayRoute -> a) a
playRoute =
    (s "play")
        </> (oneOf
                [ map ComputerPlay (s "computer")
                , map (CustomPlay << Just) (s "custom" </> string)
                , map (CustomPlay Nothing) (s "custom")
                , map QuickPlay (s "quickplay")
                ]
            )


specRoute : Parser (String -> a) a
specRoute =
    s "spec" </> string


labRoute : Parser a a
labRoute =
    s "lab"
