module Notifications.State exposing (init, receive, update)

import Notifications.Messages exposing (Msg(..))
import Notifications.Types exposing (Model, Notification)
import Util exposing (splitOnColon)


init : Model
init =
    { notifications = [] }


receive : String -> Model -> Model
receive msg model =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "systemMessage" ->
            { model
                | notifications =
                    notif content :: model.notifications
            }

        _ ->
            model


update : Model -> Msg -> Model
update model msg =
    case msg of
        Dismiss ->
            { model
                | notifications = List.drop 1 model.notifications
            }


notif : String -> Notification
notif str =
    { text = str }
