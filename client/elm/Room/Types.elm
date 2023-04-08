module Room.Types exposing (Model(..))

import Connected.Types as Connected
import Create.Types as Create
import Entrypoint.Types as Entrypoint
import Feedback.Types as Feedback
import Leaderboard.Types as Leaderboard
import League.Types as League
import Lobby.Types as Lobby
import Login.Types as Login
import Profile.Types as Profile
import Replay.Types as Replay
import Signup.Types as Signup


type Model
    = Lobby Lobby.Model
    | Connected Connected.Model
    | Replay Replay.Model
    | Login Login.Model
    | Signup Signup.Model
    | Feedback Feedback.Model
    | League League.Model
    | Leaderboard Leaderboard.Model
    | Profile Profile.Model
    | Entrypoint Entrypoint.Model
    | Create Create.Model
