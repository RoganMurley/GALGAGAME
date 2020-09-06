module Main.Messages exposing (Msg(..))

import Assets.Messages as Assets
import Browser exposing (UrlRequest)
import Http
import Keyboard exposing (Key)
import Mouse
import Room.Messages as Room
import Settings.Messages as Settings
import Settings.Types exposing (VolumeType)
import Url exposing (Url)


type Msg
    = AssetsMsg Assets.Msg
    | CopyInput String
    | Frame Float
    | GetAuth
    | GetAuthCallback (Result Http.Error (Maybe String))
    | KeyPress Key
    | Logout
    | LogoutCallback (Result Http.Error ())
    | MousePosition Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | Receive String
    | Resize Int Int
    | RoomMsg Room.Msg
    | SelectAllInput String
    | Send String
    | SettingsMsg Settings.Msg
    | SetVolume VolumeType Int
    | TouchPosition (Maybe Mouse.Position)
    | UrlChange Url
    | UrlRequest UrlRequest
    | GodCommand String
