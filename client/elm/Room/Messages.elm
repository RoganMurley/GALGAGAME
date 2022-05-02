module Room.Messages exposing (Msg(..))

import Connected.Messages as Connected
import Feedback.Messages as Feedback
import Leaderboard.Messages as Leaderboard
import League.Messages as League
import Lobby.Messages as Lobby
import Login.Messages as Login
import Menu.Messages as Menu
import Mode exposing (Mode)
import Profile.Messages as Profile
import Replay.Messages as Replay
import Signup.Messages as Signup


type Msg
    = ConnectedMsg Connected.Msg
    | LobbyMsg Lobby.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | ReplayMsg Replay.Msg
    | SignupMsg Signup.Msg
    | FeedbackMsg Feedback.Msg
    | LeagueMsg League.Msg
    | LeaderboardMsg Leaderboard.Msg
    | ProfileMsg Profile.Msg
    | StartGame Mode (Maybe String)
