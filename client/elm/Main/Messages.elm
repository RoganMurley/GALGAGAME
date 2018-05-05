module Main.Messages exposing (Msg(..))

import Http
import Mouse
import Navigation exposing (Location)
import Room.Messages as Room
import Settings.Messages as Settings
import Settings.Types exposing (VolumeType)
import Texture.Messages as Texture


type Msg
    = CopyInput String
    | Frame Float
    | GetAuth
    | GetAuthCallback (Result Http.Error (Maybe String))
    | Logout
    | LogoutCallback (Result Http.Error ())
    | MousePosition Mouse.Position
    | Receive String
    | Resize Int Int
    | RoomMsg Room.Msg
    | SelectAllInput String
    | Send String
    | SettingsMsg Settings.Msg
    | SetVolume VolumeType Int
    | TextureMsg Texture.Msg
    | UrlChange Location
