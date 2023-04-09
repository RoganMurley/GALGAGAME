module Notifications.Types exposing (Model, Notification)

import Main.Messages as Main


type alias Model =
    { notifications : List Notification
    }


type alias Notification =
    { text : String
    , timer : Float
    , callback : Maybe Main.Msg
    }
