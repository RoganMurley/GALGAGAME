module Notifications.State exposing (init, receive, tick, update)

import Main.Messages exposing (Msg(..))
import Notifications.Messages exposing (Msg(..))
import Notifications.Types exposing (Model, Notification)
import String exposing (toUpper)
import Util exposing (splitOnColon, splitOnComma)


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

        "challengedBy" ->
            let
                ( opponentName, roomId ) =
                    splitOnComma content

                newNotification =
                    { text = "Challenged by " ++ toUpper opponentName
                    , timer = 0
                    , callback = Just <| GotoChallengeGame (Just roomId)
                    }
            in
            { model
                | notifications =
                    newNotification :: model.notifications
            }

        _ ->
            model


tick : Model -> Float -> Model
tick model dt =
    let
        newNotifications =
            case model.notifications of
                n :: ns ->
                    { n | timer = n.timer + dt } :: ns

                [] ->
                    []
    in
    { model | notifications = newNotifications }


update : Model -> Msg -> Model
update model msg =
    case msg of
        Dismiss ->
            { model
                | notifications = List.drop 1 model.notifications
            }


notif : String -> Notification
notif str =
    { text = str, timer = 0, callback = Nothing }
