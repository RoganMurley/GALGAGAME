module Notifications.State exposing (init, receive, tick, update)

import Main.Messages exposing (Msg(..))
import Notifications.Messages exposing (Msg(..))
import Notifications.Types exposing (Model, Notification)
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
                    { text = "Challenged by " ++ opponentName ++ "! Tap to fight!"
                    , timer = Just 50000
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
    case model.notifications of
        n :: ns ->
            case n.timer of
                Nothing ->
                    model

                Just t ->
                    if t - dt < 0 then
                        -- Expired
                        { model | notifications = ns }

                    else
                        { model | notifications = { n | timer = Just <| t - dt } :: ns }

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
    { text = str, timer = Nothing, callback = Nothing }
