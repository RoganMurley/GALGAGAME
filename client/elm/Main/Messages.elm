module Main.Messages exposing (Msg(..))

import Browser exposing (UrlRequest)
import Http
import Keyboard exposing (Key)
import Mouse
import Room.Messages as Room
import Settings.Messages as Settings
import Settings.Types exposing (VolumeType)
import Texture.Messages as Texture
import Url exposing (Url)


type Msg
    = CopyInput String
    | Frame Float
    | GetAuth
    | GetAuthCallback (Result Http.Error (Maybe String))
    | KeyPress Key
    | Logout
    | LogoutCallback (Result Http.Error ())
    | MousePosition Mouse.Position
    | MouseClick Mouse.Position
    | Receive String
    | Resize Int Int
    | RoomMsg Room.Msg
    | SelectAllInput String
    | Send String
    | SettingsMsg Settings.Msg
    | SetVolume VolumeType Int
    | TextureMsg Texture.Msg
    | TouchPosition (Maybe Mouse.Position)
    | UrlChange Url
    | UrlRequest UrlRequest
    | GodCommand String
