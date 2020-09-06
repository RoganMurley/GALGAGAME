module PlayState.State exposing (carry, get, map, mouseClick, mouseUp, resolveOutcomeStr, tick, update)

import Animation.Types exposing (Anim(..))
import Assets.State as Assets
import Assets.Types as Assets
import Audio.State exposing (playSound)
import Browser.Navigation
import Buttons.State as Buttons
import Collision exposing (hitTest3d)
import Connected.Messages as Connected
import Endgame.WebGL as Endgame
import Game.State as Game
import Game.Types as Game
import GameState.Messages as GameState
import GameType exposing (GameType(..))
import Holding.Types exposing (Holding(..))
import Hover exposing (encodeHoverSelf)
import Json.Decode as Json
import List.Extra as List
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode)
import Model.Decoders as Model
import Model.Diff exposing (Diff, initDiff)
import Model.State as Model
import Model.Types exposing (Model)
import Mouse exposing (Position)
import PlayState.Decoders as PlayState
import PlayState.Messages as PlayState exposing (Msg(..), PlayingOnly(..), TurnOnly(..))
import PlayState.Types as PlayState exposing (PlayState(..), ResolveOutcomeInput)
import Ports exposing (websocketSend)
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Result
import Room.Messages as Room
import Util exposing (message)
import WhichPlayer.Types exposing (WhichPlayer(..))


update : Msg -> PlayState -> Mode -> Assets.Model -> ( PlayState, Cmd Main.Msg )
update msg state mode assets =
    case msg of
        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode assets

        HoverOtherOutcome otherHover ->
            case state of
                Playing ({ game } as playing) ->
                    ( Playing { playing | game = { game | otherHover = otherHover } }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        DamageOutcome dmg ->
            case state of
                Playing ({ game } as playing) ->
                    let
                        hover =
                            Game.hoverDamage game.hover dmg
                    in
                    ( Playing { playing | game = { game | hover = hover } }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        GotoComputerGame ->
            ( state, Browser.Navigation.load <| "/play/computer" )

        GotoReplay replayId ->
            ( state, Browser.Navigation.load <| "/replay/" ++ replayId )

        ReplaySaved replayId ->
            case state of
                Ended ended ->
                    ( Ended { ended | replayId = Just replayId }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        StatChange statChange ->
            case state of
                Ended ended ->
                    ( Ended { ended | xp = Just statChange }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        ClickFeedback pos ->
            ( map (\game -> { game | feedback = { progress = 1000, pos = pos } :: game.feedback }) state, Cmd.none )


updatePlayingOnly : PlayingOnly -> PlayState -> Mode.Mode -> Assets.Model -> ( PlayState, Cmd Main.Msg )
updatePlayingOnly msg state mode assets =
    case mode of
        Mode.Spectating ->
            ( state, Cmd.none )

        Mode.Playing ->
            case msg of
                Rematch ->
                    case state of
                        Ended _ ->
                            ( state, websocketSend "rematch:" )

                        _ ->
                            ( state, Cmd.none )

                HoverCard hover ->
                    let
                        sound =
                            case Game.getHoverIndex hover of
                                Just _ ->
                                    playSound assets.audio "sfx/hover.mp3"

                                Nothing ->
                                    Cmd.none
                    in
                    ( state
                    , Cmd.batch
                        [ message <|
                            Main.Send <|
                                "hover:"
                                    ++ encodeHoverSelf hover
                        , sound
                        ]
                    )

                IllegalPass ->
                    case state of
                        Playing { game } ->
                            let
                                -- Construct the ResolveData clientside to avoid latency.
                                newState : PlayState
                                newState =
                                    resolveOutcome
                                        (Just state)
                                        { initial = initial, resDiffList = resDiffList, finalState = state }

                                initial : Model
                                initial =
                                    game.res.final

                                resDiffList : List Resolvable.ResolveDiffData
                                resDiffList =
                                    [ { diff = initDiff
                                      , anim = HandFullPass
                                      , animDamage = ( 0, 0 )
                                      , stackCard = Nothing
                                      }
                                    ]
                            in
                            ( newState
                            , Cmd.none
                            )

                        _ ->
                            ( state
                            , Cmd.none
                            )

                TurnOnly turnOnly ->
                    updateTurnOnly turnOnly state assets


updateTurnOnly : TurnOnly -> PlayState -> Assets.Model -> ( PlayState, Cmd Main.Msg )
updateTurnOnly msg state { audio } =
    case state of
        Playing { game } ->
            if game.res.final.turn == PlayerA then
                case msg of
                    EndTurn ->
                        let
                            -- Set passed to True to avoid latency.
                            newState : PlayState
                            newState =
                                Playing { game = { game | passed = True } }
                        in
                        ( newState
                        , Cmd.batch
                            [ websocketSend "end:"
                            , playSound audio "sfx/endTurn.mp3"
                            ]
                        )

                    PlayCard card index pos ->
                        let
                            -- Construct the ResolveData clientside to avoid latency.
                            newState : PlayState
                            newState =
                                resolveOutcome
                                    (Just state)
                                    { initial = initial, resDiffList = resDiffList, finalState = finalState }
                                    |> map (\g -> { g | holding = NoHolding })

                            initial : Model
                            initial =
                                game.res.final

                            playDiff : Diff
                            playDiff =
                                { initDiff
                                    | turn = Just PlayerB
                                    , stack = Just <| { owner = PlayerA, card = card } :: initial.stack
                                    , hand = Just <| List.removeAt index initial.hand
                                }

                            windupDiff : Diff
                            windupDiff =
                                { initDiff | rot = Just <| initial.rot + 1 }

                            resDiffList : List Resolvable.ResolveDiffData
                            resDiffList =
                                [ { diff = playDiff
                                  , anim = Play PlayerA card index (Just pos)
                                  , animDamage = ( 0, 0 )
                                  , stackCard = Nothing
                                  }
                                , { diff = windupDiff
                                  , anim = Windup PlayerA
                                  , animDamage = ( 0, 0 )
                                  , stackCard = Nothing
                                  }
                                ]

                            final : Model
                            final =
                                List.foldl Model.Diff.merge initial [ playDiff, windupDiff ]

                            finalState : PlayState
                            finalState =
                                Playing { game = Game.gameInit final }
                        in
                        ( newState
                        , websocketSend <| "play:" ++ String.fromInt index
                        )

                    HoldCard card index ray ->
                        ( map (Game.hold card index ray) state
                        , Cmd.none
                        )

            else
                ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


tick : Flags -> PlayState -> Float -> ( PlayState, Cmd Msg )
tick flags state dt =
    case state of
        Playing ({ game } as playing) ->
            let
                ( newGame, msg ) =
                    Game.tick flags dt game
            in
            ( Playing
                { playing
                    | game = newGame
                }
            , msg
            )

        Ended ({ game, buttons } as ended) ->
            let
                ( newGame, msg ) =
                    Game.tick flags dt game

                ( w, h ) =
                    flags.dimensions

                params =
                    { time = flags.time
                    , w = w
                    , h = h
                    , pixelRatio = flags.pixelRatio
                    }

                newButtons =
                    Endgame.buttonEntities params buttons dt flags.mouse
            in
            ( Ended
                { ended
                    | game = newGame
                    , buttons = newButtons
                }
            , msg
            )


map : (Game.Model -> Game.Model) -> PlayState -> PlayState
map f state =
    set (f <| get identity state) state


get : (Game.Model -> a) -> PlayState -> a
get g state =
    case state of
        Playing { game } ->
            g game

        Ended { game } ->
            g game


set : Game.Model -> PlayState -> PlayState
set game state =
    case state of
        Playing playing ->
            Playing { playing | game = game }

        Ended ended ->
            Ended { ended | game = game }


carry : PlayState -> PlayState -> PlayState
carry old new =
    map
        (\game ->
            { game
                | res = get .res new
                , entities = get .entities old
                , focus = get .focus old
                , vfx = get .vfx old
                , buttons = get .buttons old
                , holding = get .holding old
            }
        )
        new


resolveOutcomeStr : String -> Maybe PlayState -> Result Json.Error PlayState
resolveOutcomeStr str mState =
    Result.map (resolveOutcome mState) <|
        Json.decodeString PlayState.resolveOutcomeInputDecoder str


resolveOutcome : Maybe PlayState -> ResolveOutcomeInput -> PlayState
resolveOutcome mState { initial, resDiffList, finalState } =
    let
        state : PlayState
        state =
            Maybe.withDefault
                (Playing { game = Game.gameInit Model.init })
                mState

        oldResList : List Resolvable.ResolveData
        oldResList =
            get (.res >> .resList) state

        oldTick : Float
        oldTick =
            case oldResList of
                [] ->
                    0

                _ ->
                    get (.res >> .tick) state

        resList : List Resolvable.ResolveData
        resList =
            Resolvable.resDiffToData initial resDiffList

        model : Model
        model =
            get (.res >> .final) finalState

        res : Resolvable.Model
        res =
            { tick = oldTick
            , final = model
            , resList = oldResList ++ resList
            }

        newState : PlayState
        newState =
            map (\game -> { game | res = res }) finalState
    in
    carry state newState


mouseClick : Flags -> Assets.Model -> GameType -> Mode -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseClick { dimensions } assets gameType mode { x, y } state =
    let
        pos =
            vec2 (toFloat x) (toFloat y)

        game =
            get identity state

        msg =
            case mHandEntity of
                Just { card, index } ->
                    PlayingOnly <| TurnOnly <| HoldCard card index ctx.mouseRay

                Nothing ->
                    ClickFeedback pos

        ( newPlayState, newMsg ) =
            update
                msg
                state
                mode
                assets

        ctx =
            Game.bareContextInit dimensions Assets.init (Just pos)

        mHandEntity =
            ctx.mouseRay
                |> Maybe.andThen
                    (\ray ->
                        List.find (hitTest3d ray 0.12) game.entities.hand
                    )

        -- Endgame
        playMsg =
            message
                << Main.RoomMsg
                << Room.ConnectedMsg
                << Connected.GameStateMsg
                << GameState.PlayStateMsg

        buttonMsg =
            case state of
                Playing _ ->
                    case Buttons.hit game.buttons of
                        Just ( key, _ ) ->
                            case key of
                                "go" ->
                                    playMsg <|
                                        PlayingOnly <|
                                            PlayState.TurnOnly PlayState.EndTurn

                                "goDisabled" ->
                                    playMsg <|
                                        PlayState.PlayingOnly PlayState.IllegalPass

                                _ ->
                                    Cmd.none

                        Nothing ->
                            Cmd.none

                Ended { buttons, replayId, winner } ->
                    case Buttons.hit buttons of
                        Just ( key, _ ) ->
                            case key of
                                "playAgain" ->
                                    case ( gameType, winner ) of
                                        ( TutorialGame, Just PlayerA ) ->
                                            playMsg PlayState.GotoComputerGame

                                        _ ->
                                            playMsg <| PlayState.PlayingOnly PlayState.Rematch

                                "watchReplay" ->
                                    case replayId of
                                        Just r ->
                                            playMsg <| PlayState.GotoReplay r

                                        Nothing ->
                                            Cmd.none

                                _ ->
                                    Cmd.none

                        Nothing ->
                            Cmd.none
    in
    ( newPlayState
    , Cmd.batch
        [ newMsg
        , buttonMsg
        ]
    )


mouseUp : Flags -> Assets.Model -> GameType -> Mode -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseUp _ assets _ mode _ state =
    let
        game =
            get identity state

        mMsg =
            case game.holding of
                Holding { card, handIndex, pos } ->
                    Just <| PlayingOnly <| TurnOnly <| PlayCard card handIndex pos

                NoHolding ->
                    Nothing

        ( newPlayState, cmd ) =
            case mMsg of
                Just msg ->
                    update
                        msg
                        state
                        mode
                        assets

                Nothing ->
                    ( state, Cmd.none )
    in
    ( newPlayState, cmd )
