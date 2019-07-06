module Room.Types exposing (Model(..))

import Connected.Types as Connected
import Feedback.Types as Feedback
import Lobby.Types as Lobby
import Login.Types as Login
import Replay.Types as Replay
import Signup.Types as Signup


type Model
    = MainMenu
    | Lobby Lobby.Model
    | Connected Connected.Model
    | Replay Replay.Model
    | Login Login.Model
    | Signup Signup.Model
    | Feedback Feedback.Model
