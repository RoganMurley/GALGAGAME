module Compass.State exposing (..)

import Compass.Types exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, top, map, oneOf)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Play playRoute
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
