module Main exposing (..)

import Char exposing (isLower)
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
import Messages exposing (GameMsg(..), MenuMsg(..), Msg(..))
import Random
import Random.Char exposing (char)
import Random.String exposing (string)
import Time exposing (Time, second)
import Tuple exposing (first)
import Util exposing (applyFst, message)
import Ports exposing (copyInput, selectAllInput, queryParams)


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
    , httpPort : String
    }


type alias Seed =
    Int


type RoomModel
    = MainMenu Seed
    | Connecting ConnectingModel
    | Connected ConnectedModel


type alias ConnectingModel =
    { roomID : String
    , name : String
    , error : String
    , valid : Bool
    }


type alias ConnectedModel =
    { chat : Chat.Model
    , game : GameState.GameState
    , mode : Mode
    , roomID : String
    }


type alias Flags =
    { hostname : String
    , httpPort : String
    , play : Maybe String
    , seed : Seed
    }


type Mode
    = Spectating
    | Playing


init : Flags -> ( Model, Cmd Msg )
init ({ hostname, httpPort, play, seed } as flags) =
    let
        model : Model
        model =
            { room =
                case play of
                    Just roomID ->
                        connectingInit
                            ("player" ++ (first (Random.step usernameNumberGenerator (Random.initialSeed seed))))
                            roomID

                    Nothing ->
                        MainMenu seed
            , hostname = hostname
            , httpPort = httpPort
            }
    in
        ( model, Cmd.none )


connectingInit : String -> String -> RoomModel
connectingInit username roomID =
    Connecting
        { roomID = roomID
        , name = username
        , error = ""
        , valid = True
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ hostname, room } as model) =
    case room of
        MainMenu seed ->
            case msg of
                MainMenuMsg m ->
                    case m of
                        MenuCustom ->
                            ( { model
                                | room =
                                    (connectingInit
                                        ("player" ++ (first (Random.step usernameNumberGenerator (Random.initialSeed seed))))
                                        (first (Random.step roomIDGenerator (Random.initialSeed seed)))
                                    )
                              }
                            , Cmd.none
                            )

                otherwise ->
                    ( model, Cmd.none )

        Connecting ({ roomID } as connectingModel) ->
            case msg of
                Play ->
                    ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Playing, roomID = roomID } }, queryParams ("?play=" ++ roomID) )

                Spectate ->
                    ( { model | room = Connected { chat = Chat.init, game = Waiting, mode = Spectating, roomID = roomID } }, queryParams ("?play=" ++ roomID) )

                otherwise ->
                    applyFst (\c -> { model | room = Connecting c }) (connectingUpdate hostname msg connectingModel)

        Connected connectedModel ->
            applyFst (\c -> { model | room = Connected c }) (connectedUpdate hostname msg connectedModel)


connectingUpdate : String -> Msg -> ConnectingModel -> ( ConnectingModel, Cmd Msg )
connectingUpdate hostname msg ({ roomID, name, error, valid } as model) =
    case msg of
        Input input ->
            ( { model | name = input, error = Tuple.second (validateName input), valid = Tuple.first (validateName input) }, Cmd.none )

        Send str ->
            ( model, Cmd.batch [ send hostname (Debug.log "sending" str), send hostname (Debug.log "sending" ("room:" ++ roomID)) ] )

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
                    ( { model | name = "" }, message (Send ("play:" ++ name)) )

        KeyPress _ ->
            ( model, Cmd.none )

        Tick t ->
            ( model, Cmd.none )

        SelectAllInput elementId ->
            ( model, selectAllInput elementId )

        CopyInput elementId ->
            ( model, copyInput elementId )

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
                Ended which _ ->
                    ( model, playingOnly model (send hostname "rematch:") )

                otherwise ->
                    ( model, Cmd.none )

        HoverCard name ->
            let
                cardName =
                    case name of
                        Just x ->
                            x

                        Nothing ->
                            "null"
            in
                ( model, playingOnly model (message (Send ("hover:" ++ cardName))) )

        SelectAllInput elementId ->
            ( model, selectAllInput elementId )

        CopyInput elementId ->
            ( model, copyInput elementId )

        otherwise ->
            Debug.crash "Unexpected action while connected ;_;"


connectedReceive : ConnectedModel -> String -> ( ConnectedModel, Cmd Msg )
connectedReceive model msg =
    if (startsWith "chat:" msg) then
        ( model, message (NewChatMsg (dropLeft (length "chat:") msg)) )
    else if (startsWith "sync:" msg) then
        ( model, message (GameStateMsg (Sync (dropLeft (length "sync:") msg))) )
    else if (startsWith "hover:" msg) then
        ( model, message (GameStateMsg (HoverOutcome (parseHoverOutcome (dropLeft (length "hover:") msg)))) )
    else if (startsWith "res:" msg) then
        ( model, message (GameStateMsg (ResolveOutcome (dropLeft (length "res:") msg))) )
    else
        Debug.crash ("Error decoding message from server: " ++ msg)


parseHoverOutcome : String -> Maybe Int
parseHoverOutcome msg =
    case msg of
        "null" ->
            Nothing

        otherwise ->
            case String.toInt msg of
                Ok index ->
                    Just index

                Err err ->
                    Debug.crash err


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



-- OTHER


roomIDGenerator : Random.Generator String
roomIDGenerator =
    string 8 Random.Char.english


usernameNumberGenerator : Random.Generator String
usernameNumberGenerator =
    string 3 (char 48 57)


validateName : String -> ( Bool, String )
validateName name =
    if length name > 20 then
        ( False, "username too long" )
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
view ({ hostname, httpPort } as model) =
    case model.room of
        MainMenu _ ->
            div [ class "main-menu" ]
                [ h1 [] [ text "VANA" ]
                , button
                    [ class "menu-button", disabled True ]
                    [ text "Quickplay" ]
                , button
                    [ class "menu-button", onClick (MainMenuMsg MenuCustom) ]
                    [ text "Custom" ]
                ]

        Connected { chat, game, roomID } ->
            div []
                [ Chat.view chat
                , GameState.stateView game roomID hostname httpPort
                ]

        Connecting { name, error, valid } ->
            div [ class "connecting-box" ]
                [ h1 [] [ text "VANA : Custom Game" ]
                , div []
                    [ div [ class "input-group" ]
                        [ input [ onInput Input, placeholder "username", value name, id "playername-input", onClick (SelectAllInput "playername-input") ] []
                        , button [ onClick (Send ("play:" ++ name)), disabled (not valid) ] [ text "Play" ]
                        , button [ onClick (Send ("spectate:" ++ name)), disabled (not valid) ] [ text "Spec" ]
                        ]
                    , div [ class "error" ] [ text error ]
                    ]
                ]
