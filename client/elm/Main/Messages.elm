module Main.Messages exposing (Msg(..))

import Assets.Messages as Assets
import Browser exposing (UrlRequest)
import Browser.Events exposing (Visibility)
import Http
import Keyboard exposing (Key)
import Mouse
import Notifications.Messages as Notifications
import Room.Messages as Room
import Settings.Messages as Settings
import Settings.Types exposing (VolumeType)
import Url exposing (Url)


type Msg
    = AssetsMsg Assets.Msg
    | CopyInput String
    | Frame Float
    | KeyPress Key
    | Logout
    | LogoutCallback (Result Http.Error ())
    | MousePosition Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | NotificationsMsg Notifications.Msg
    | Receive String
    | Reload
    | Resize Int Int
    | RoomMsg Room.Msg
    | SelectAllInput String
    | Send String
    | SettingsMsg Settings.Msg
    | SetScaling Float
    | SetUsername String
    | SetVolume VolumeType Int
    | TouchPosition (Maybe Mouse.Position)
    | UrlChange Url
    | UrlRequest UrlRequest
    | GodCommand String
    | NoOp
    | VisibilityChange Visibility
