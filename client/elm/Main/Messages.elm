module Main.Messages exposing (Msg(..))

import Navigation exposing (Location)
import Room.Messages as Room
import Settings.Messages as Settings


type Msg
    = CopyInput String
    | Frame Float
    | Receive String
    | Resize Int Int
    | RoomMsg Room.Msg
    | SelectAllInput String
    | Send String
    | SettingsMsg Settings.Msg
    | SetVolume Int
    | UrlChange Location
