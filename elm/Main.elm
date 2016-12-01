module Main exposing (..)

import Char exposing (isLower)


-- import Cmd.Extra exposing (message)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Mouse
import String exposing (dropLeft, length, startsWith)
import WebSocket
import Chat exposing (addChatMessage)
import Drag exposing (dragAt, dragEnd, dragStart, getPosition)
import GameState exposing (Card, GameState(..), Hand, Model, Turn, WhichPlayer(..), resTick, stateUpdate, stateView, tickForward, tickZero, view)
import Messages exposing (GameMsg(..), Msg(..))
import Task
import Time exposing (Time, second)
import Util exposing (applyFst)


message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { room : RoomModel
    , hostname : String
    }


type RoomModel
    = Connecting ConnectingModel
    | Connected ConnectedModel


type alias ConnectingModel =
    { name : String
    , error : String
    , valid : Bool
    }


type alias ConnectedModel =
    { chat : Chat.Model
    , game : GameState.GameState
    , mode : Mode
    }


type alias Flags =
    { hostname : String
    }


type Mode
    = Spectating
    | Playing


init : Flags -> ( Model, Cmd Msg )
init { hostname } =
    let
        model : Model
        model =
            { room = Connecting { name = "", error = "", valid = False }
            , hostname = hostname
            }
    in
        ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ hostname, room } as model) =
    case room of
        Connecting connectingModel ->
            case msg of
                Play ->
                    ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Playing } }, Cmd.none )

                Spectate ->
                    ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Spectating } }, Cmd.none )

                otherwise ->
                    applyFst (\c -> { model | room = Connecting c }) (connectingUpdate hostname msg connectingModel)

        Connected connectedModel ->
            applyFst (\c -> { model | room = Connected c }) (connectedUpdate hostname msg connectedModel)


connectingUpdate : String -> Msg -> ConnectingModel -> ( ConnectingModel, Cmd Msg )
connectingUpdate hostname msg ({ name, error, valid } as model) =
    case msg of
        Input input ->
            ( { model | name = input, error = Tuple.second (validateName name), valid = Tuple.first (validateName name) }, Cmd.none )

        Send str ->
            ( model, Cmd.batch [ send hostname str, send hostname (Debug.log "sending" "room:default") ] )

        Receive str ->
            connectingReceive model str

        DragAt pos ->
            ( model, Cmd.none )

        DragEnd pos ->
            ( model, Cmd.none )

        ConnectError str ->
            ( { model | error = str }, Cmd.none )

        KeyPress 13 ->
            case Tuple.first (validateName name) of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model, message (Send ("play:" ++ name)) )

        KeyPress _ ->
            ( model, Cmd.none )

        Tick t ->
            ( model, Cmd.none )

        otherwise ->
            Debug.crash "Unexpected action while not connected ;_;"


connectedUpdate : String -> Msg -> ConnectedModel -> ( ConnectedModel, Cmd Msg )
connectedUpdate hostname msg ({ chat, game, mode } as model) =
    case msg of
        Input input ->
            ( { model | chat = { chat | input = input } }, Cmd.none )

        Send str ->
            ( { model | chat = { chat | input = "" } }
            , if str /= "chat:" then
                send hostname str
              else
                Cmd.none
            )

        Receive str ->
            connectedReceive model str

        DragStart pos ->
            ( { model | chat = dragStart chat pos }, Cmd.none )

        DragAt pos ->
            ( { model | chat = dragAt chat pos }, Cmd.none )

        DragEnd pos ->
            ( { model | chat = dragEnd chat }, Cmd.none )

        DrawCard ->
            ( model, turnOnly model (send hostname "draw:") )

        EndTurn ->
            ( model, turnOnly model (send hostname "end:") )

        PlayCard name ->
            ( model, turnOnly model (send hostname ("play:" ++ name)) )

        NewChatMsg str ->
            ( { model | chat = addChatMessage str chat }, Cmd.none )

        GameStateMsg gameMsg ->
            ( { model | game = stateUpdate gameMsg game }, Cmd.none )

        KeyPress 13 ->
            ( model, message (Send ("chat:" ++ chat.input)) )

        KeyPress _ ->
            ( model, Cmd.none )

        Tick _ ->
            ( { model | game = tickForward game }
            , if tickZero model.game then
                message ResolveStep
              else
                Cmd.none
            )

        ResolveStep ->
            ( { model | game = resTick game }, Cmd.none )

        Rematch ->
            case model.game of
                Victory which ->
                    ( model, playingOnly model (send hostname "rematch:") )

                Draw ->
                    ( model, playingOnly model (send hostname "rematch:") )

                otherwise ->
                    ( model, Cmd.none )

        otherwise ->
            Debug.crash "Unexpected action while connected ;_;"


connectedReceive : ConnectedModel -> String -> ( ConnectedModel, Cmd Msg )
connectedReceive model msg =
    if (startsWith "chat:" msg) then
        ( model, message (NewChatMsg (dropLeft (length "chat:") msg)) )
    else if (startsWith "sync:" msg) then
        ( model, message (GameStateMsg (Sync (dropLeft (length "sync:") msg))) )
    else
        Debug.crash ("Error decoding message from server: " ++ msg)


connectingReceive : ConnectingModel -> String -> ( ConnectingModel, Cmd Msg )
connectingReceive model msg =
    if (startsWith "acceptPlay:" msg) then
        ( model, message Play )
    else if (startsWith "acceptSpec:" msg) then
        ( model, message Spectate )
    else if (startsWith "error:" msg) then
        ( model, message (ConnectError (dropLeft (length "error:") msg)) )
    else
        -- Defer other messages.
        ( model, message (Receive msg) )


send : String -> String -> Cmd Msg
send hostname =
    WebSocket.send ("ws://" ++ hostname ++ ":9160")


playingOnly : ConnectedModel -> Cmd Msg -> Cmd Msg
playingOnly { mode } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            cmdMsg


turnOnly : ConnectedModel -> Cmd Msg -> Cmd Msg
turnOnly { mode, game } cmdMsg =
    case mode of
        Spectating ->
            Cmd.none

        Playing ->
            case game of
                PlayingGame model res ->
                    case model.turn of
                        PlayerA ->
                            cmdMsg

                        PlayerB ->
                            Cmd.none

                otherwise ->
                    Cmd.none



-- VALIDATION


validateName : String -> ( Bool, String )
validateName name =
    if length name > 20 then
        ( False, "Userame too long!" )
    else if String.isEmpty name then
        ( False, "" )
    else
        ( True, "" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen ("ws://" ++ model.hostname ++ ":9160") Receive
        , Mouse.moves DragAt
        , Mouse.ups DragEnd
        , Keyboard.presses KeyPress
        , Time.every (second / 60) Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.room of
        Connected { chat, game } ->
            div []
                [ Chat.view chat
                , GameState.stateView game
                ]

        Connecting { name, error, valid } ->
            div [ class "connecting-box" ]
                [ div []
                    [ input [ onInput Input, placeholder "Username" ] []
                    , button [ onClick (Send ("play:" ++ name)), disabled (not valid) ] [ text "Play" ]
                    , button [ onClick (Send ("spectate:" ++ name)), disabled (not valid) ] [ text "Spec" ]
                    ]
                , div [ class "error" ] [ text error ]
                ]
