module Notifications.Types exposing (Model, Notification)


type alias Model =
    { notifications : List Notification
    }


type alias Notification =
    { text : String
    , timer : Maybe Float
    }
