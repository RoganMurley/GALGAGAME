module Room.Messages exposing (Msg(..))

import Connected.Messages as Connected
import Create.Messages as Create
import Feedback.Messages as Feedback
import Leaderboard.Messages as Leaderboard
import League.Messages as League
import Lobby.Messages as Lobby
import Login.Messages as Login
import Mode exposing (Mode)
import Presence.Messages as Presence
import Profile.Messages as Profile
import Replay.Messages as Replay
import Signup.Messages as Signup


type Msg
    = ConnectedMsg Connected.Msg
    | LobbyMsg Lobby.Msg
    | LoginMsg Login.Msg
    | ReplayMsg Replay.Msg
    | SignupMsg Signup.Msg
    | FeedbackMsg Feedback.Msg
    | LeagueMsg League.Msg
    | LeaderboardMsg Leaderboard.Msg
    | ProfileMsg Profile.Msg
    | PresenceMsg Presence.Msg
    | CreateMsg Create.Msg
    | StartGame Mode (Maybe String)
