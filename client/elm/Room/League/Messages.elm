module League.Messages exposing (Msg(..))

import Http
import League.Types exposing (LeagueError)


type Msg
    = Submit
    | SubmitCallback (Result Http.Error (Maybe LeagueError))
    | CheckState
    | CheckStateCallback (Result Http.Error (Maybe LeagueError))
