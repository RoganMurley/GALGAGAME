module League.Types exposing (LeagueError, Model, SubmitState(..))


type alias Model =
    { error : String
    , submitState : SubmitState
    }


type alias LeagueError =
    { error : String
    }


type SubmitState
    = Waiting
    | NotSubmitted
    | Submitting
    | Submitted
