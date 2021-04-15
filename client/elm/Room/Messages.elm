module Room.Messages exposing (Msg(..))

import Connected.Messages as Connected
import Feedback.Messages as Feedback
import Lobby.Messages as Lobby
import Login.Messages as Login
import Menu.Messages as Menu
import Mode exposing (Mode)
import Replay.Messages as Replay
import Signup.Messages as Signup
import World.Messages as World


type Msg
    = ConnectedMsg Connected.Msg
    | LobbyMsg Lobby.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | ReplayMsg Replay.Msg
    | SignupMsg Signup.Msg
    | FeedbackMsg Feedback.Msg
    | WorldMsg World.Msg
    | StartGame Mode (Maybe String)
    | VisitWorld Bool
