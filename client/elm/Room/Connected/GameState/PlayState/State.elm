module PlayState.State exposing (carry, get, map, mouseDown, mouseUp, resolveOutcomeStr, tick, update)

import Aftermath.State as Aftermath
import Animation.Types exposing (Anim(..))
import Assets.State as Assets
import Assets.Types as Assets
import Audio.State exposing (playSound)
import Browser.Navigation
import Buttons.State as Buttons
import Card.State exposing (getCard)
import Card.Types exposing (KnowableCard(..))
import Chat.Messages as Chat
import Chat.Types as Chat
import Collision exposing (hitTest3d, hitTest3dTri)
import Connected.Messages as Connected
import Endgame.View as Endgame
import Game.Entity exposing (toTriangles)
import Game.State as Game
import Game.Types as Game
import GameState.Messages as GameState
import GameType exposing (GameType(..))
import Holding.State as Holding
import Holding.Types exposing (Holding(..))
import Hover exposing (Hover(..), HoverDamage(..), encodeHoverSelf)
import Json.Decode as Json
import List.Extra as List
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Mode exposing (Mode)
import Model.Decoders as Model
import Model.Diff exposing (Diff, initDiff)
import Model.State as Model
import Model.Types exposing (Model, Pass(..))
import Mouse exposing (MouseState(..), Position)
import PlayState.Decoders as PlayState
import PlayState.Messages as PlayState exposing (Msg(..), PlayingOnly(..), TurnOnly(..))
import PlayState.Types as PlayState exposing (PlayState(..), ResolveOutcomeInput)
import Players exposing (Players)
import Ports exposing (log, websocketSend)
import Quaternion
import Resolvable.State as Resolvable exposing (resolving)
import Resolvable.Types as Resolvable
import Result
import Room.Messages as Room
import Tutorial
import Util exposing (message)
import Wheel.State as Wheel
import WhichPlayer.Types exposing (WhichPlayer(..))


update : Msg -> PlayState -> Mode -> Players -> Assets.Model -> ( PlayState, Cmd Main.Msg )
update msg state mode players assets =
    case msg of
        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode players assets

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
                            Game.hoverDamage
                                game.hover
                                dmg

                        holding =
                            Holding.setDamage game.holding dmg
                    in
                    ( Playing
                        { playing
                            | game =
                                { game
                                    | hover = hover
                                    , holding = holding
                                }
                        }
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
                    ( Ended { ended | aftermath = Aftermath.fromStatChange statChange }
                    , Aftermath.saveUnlocks statChange
                    )

                _ ->
                    ( state, Cmd.none )

        SetLeaderboard leaderboard ->
            case state of
                Ended ended ->
                    ( Ended
                        { ended
                            | aftermath = Aftermath.fromLeaderboard ended.aftermath leaderboard
                        }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        ServerTimeLeft timeLeft ->
            ( map (\game -> { game | timeLeft = Just timeLeft }) state, Cmd.none )

        SkipAftermath ->
            case state of
                Ended ended ->
                    ( Ended { ended | aftermath = Aftermath.skip ended.aftermath }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


updatePlayingOnly : PlayingOnly -> PlayState -> Mode.Mode -> Players -> Assets.Model -> ( PlayState, Cmd Main.Msg )
updatePlayingOnly msg state mode players assets =
    case mode of
        Mode.Spectating ->
            ( state, Cmd.none )

        Mode.Playing ->
            case msg of
                Continue ->
                    case state of
                        Ended _ ->
                            let
                                newMsg =
                                    if Players.shouldRematch players then
                                        websocketSend "rematch:"

                                    else
                                        message << Main.RoomMsg <| Room.StartGame Mode.Playing Nothing
                            in
                            ( state, newMsg )

                        _ ->
                            ( state, Cmd.none )

                HoverCard hover ->
                    let
                        sound =
                            case Game.getHoverIndex hover of
                                Just _ ->
                                    -- playSound assets.audio "sfx/hover.mp3"
                                    Cmd.none

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
                            if resolving game.res then
                                ( state, Cmd.none )

                            else
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

                TickleWheel index ->
                    case state of
                        Playing { game } ->
                            let
                                { vfx } =
                                    game

                                mag =
                                    case vfx.tickle of
                                        Nothing ->
                                            100

                                        Just ( _, t ) ->
                                            max (t + 100) 1000

                                newGame =
                                    { game | vfx = { vfx | tickle = Just ( index, mag ) } }
                            in
                            ( Playing { game = newGame }, playSound assets.audio "sfx/draw.mp3" )

                        _ ->
                            ( state
                            , Cmd.none
                            )


updateTurnOnly : TurnOnly -> PlayState -> Assets.Model -> ( PlayState, Cmd Main.Msg )
updateTurnOnly msg state { audio } =
    case state of
        Playing ({ game } as playing) ->
            if game.res.final.turn == PlayerA then
                case msg of
                    EndTurn ->
                        let
                            newButtons =
                                Buttons.update
                                    "go"
                                    (\b -> { b | hover = 0 })
                                    game.buttons

                            -- Set passed to true to avoid latency.
                            newState : PlayState
                            newState =
                                Playing
                                    { game =
                                        { game
                                            | passed = True
                                            , buttons = newButtons
                                        }
                                    }
                        in
                        ( newState |> tutorialAction Tutorial.ActionPressGo
                        , Cmd.batch
                            [ websocketSend "end:"
                            , playSound audio "sfx/click.mp3"
                            ]
                        )

                    PlayCard card index pos ->
                        let
                            isCardInHandAfterResolution : Bool
                            isCardInHandAfterResolution =
                                Just card == Maybe.map getCard (List.getAt index game.res.final.hand)

                            isPremovable : Bool
                            isPremovable =
                                Resolvable.isPremovable game.res
                        in
                        if isCardInHandAfterResolution && isPremovable then
                            let
                                -- Construct the ResolveData clientside to avoid latency.
                                newState : PlayState
                                newState =
                                    resolveOutcome
                                        (Just (Playing { playing | game = resolvedGame }))
                                        { initial = initial, resDiffList = resDiffList, finalState = finalState }
                                        |> map (\g -> { g | holding = NoHolding })

                                currentModel : Model
                                currentModel =
                                    game.res.final

                                initial : Model
                                initial =
                                    { currentModel | passes = OnePass }

                                { stack } =
                                    initial

                                playDiffStack =
                                    { stack | wheel0 = Just { owner = PlayerA, card = card } }

                                playDiff : Diff
                                playDiff =
                                    { initDiff
                                        | turn = Just PlayerB
                                        , stack = Just playDiffStack
                                        , hand = Just <| List.removeAt index initial.hand
                                    }

                                windupDiff : Diff
                                windupDiff =
                                    { initDiff
                                        | rot = Just <| initial.rot + 1
                                        , stack = Just <| Wheel.fwrd playDiffStack
                                    }

                                resDiffList : List Resolvable.ResolveDiffData
                                resDiffList =
                                    [ { diff = playDiff
                                      , anim = Play PlayerA (UnknownCard card) index (Just pos)
                                      , animDamage = ( 0, 0 )
                                      }
                                    , { diff = windupDiff
                                      , anim = Windup PlayerA
                                      , animDamage = ( 0, 0 )
                                      }
                                    ]

                                final : Model
                                final =
                                    List.foldl Model.Diff.merge initial [ playDiff, windupDiff ]

                                finalState : PlayState
                                finalState =
                                    Playing { game = Game.gameInit final }

                                resolvedGame : Game.Model
                                resolvedGame =
                                    let
                                        resolvedRes =
                                            game.res
                                    in
                                    { game | res = { resolvedRes | resList = [] } }
                            in
                            ( newState |> tutorialAction Tutorial.ActionDragACard
                            , websocketSend <| "play:" ++ String.fromInt index
                            )

                        else
                            let
                                newGame =
                                    { game | holding = NoHolding }
                            in
                            ( Playing { game = newGame }, Cmd.none )

                    HoldCard card index ray ->
                        let
                            dmg =
                                Hover.getDmg game.hover

                            newState =
                                map
                                    (Game.hold card index ray dmg)
                                    state

                            -- On mobile hovers aren't triggered, so send a hover event
                            -- if one wasn't triggered.
                            newMsg =
                                case game.hover of
                                    HoverHand _ ->
                                        Cmd.none

                                    _ ->
                                        message <|
                                            Main.Send <|
                                                "hover:"
                                                    ++ encodeHoverSelf (HoverHand { index = index, tick = 0, dmg = ( HoverDamage 0, HoverDamage 0 ) })
                        in
                        ( newState, newMsg )

                    UnholdCard ->
                        let
                            newState =
                                map (\g -> { g | holding = NoHolding }) state
                        in
                        ( newState, Cmd.none )

            else
                ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


tick : Flags -> PlayState -> Chat.Model -> GameType -> Float -> ( PlayState, Cmd Msg )
tick flags state chat gameType dt =
    case state of
        Playing ({ game } as playing) ->
            let
                ( newGame, msg ) =
                    Game.tick flags dt game chat
            in
            ( Playing
                { playing
                    | game = newGame
                }
            , msg
            )

        Ended ({ game, buttons, aftermath } as ended) ->
            let
                disableMouse : Flags -> Flags
                disableMouse f =
                    { f | mouse = NoMouse }

                ( newGame, msg ) =
                    Game.tick
                        (if resolving game.res then
                            flags

                         else
                            disableMouse flags
                        )
                        dt
                        game
                        chat

                ( w, h ) =
                    flags.dimensions

                params =
                    { time = flags.time
                    , w = w
                    , h = h
                    , pixelRatio = flags.pixelRatio
                    , scaling = flags.scaling
                    }

                newButtons =
                    if resolving game.res || Aftermath.aftermathing aftermath then
                        Buttons.empty

                    else
                        Endgame.buttonEntities
                            params
                            buttons
                            gameType
                            dt
                            flags.mouse

                ( newAftermath, aftermathMsg ) =
                    if resolving game.res then
                        ( aftermath, Cmd.none )

                    else
                        Aftermath.tick dt aftermath
            in
            ( Ended
                { ended
                    | game = newGame
                    , buttons = newButtons
                    , aftermath = newAftermath
                }
            , Cmd.batch [ msg, aftermathMsg ]
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
                , tutorial = get .tutorial old
                , passive = get .passive old
                , cpu = get .cpu old
            }
        )
        new


resolveOutcomeStr : String -> Maybe PlayState -> Result Json.Error PlayState
resolveOutcomeStr str mState =
    Result.map (\r -> resolveOutcome mState r) <|
        Json.decodeString PlayState.resolveOutcomeInputDecoder str


resolveOutcome : Maybe PlayState -> ResolveOutcomeInput -> PlayState
resolveOutcome mState { initial, resDiffList, finalState } =
    let
        state : PlayState
        state =
            Maybe.withDefault
                (Playing { game = Game.gameInit Model.init })
                mState

        old : Resolvable.Model
        old =
            get .res state

        oldTick : Float
        oldTick =
            case old.resList of
                [] ->
                    0

                _ ->
                    old.tick

        resList : List Resolvable.ResolveData
        resList =
            prunePassiveAnims <|
                Resolvable.resDiffToData initial resDiffList

        model : Model
        model =
            get (.res >> .final) finalState

        res : Resolvable.Model
        res =
            { tick = oldTick
            , final = model
            , resList = old.resList ++ resList
            , history = old.history
            }

        newState : PlayState
        newState =
            map (\game -> { game | res = res }) finalState

        prunePassiveAnims : List Resolvable.ResolveData -> List Resolvable.ResolveData
        prunePassiveAnims =
            case mState of
                Just (Playing { game }) ->
                    if game.passive then
                        List.filter
                            (\r ->
                                case r.anim of
                                    Pass _ ->
                                        False

                                    _ ->
                                        True
                            )

                    else
                        identity

                _ ->
                    identity
    in
    carry state newState


mouseDown : Flags -> Assets.Model -> GameType -> Mode -> Players -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseDown { dimensions, mouse } assets _ mode players { x, y } state =
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
                    case mWheelEntity of
                        Just { index } ->
                            PlayingOnly <| TickleWheel index

                        Nothing ->
                            NoOp

        ( newPlayState, newMsg ) =
            update
                msg
                state
                mode
                players
                assets

        ctx =
            Game.bareContextInit dimensions Assets.init mouse

        mHandEntity =
            ctx.mouseRay
                |> Maybe.andThen
                    (\ray ->
                        List.find (hitTest3d ray 0.1) <| List.reverse game.entities.hand
                    )

        mWheelEntity =
            ctx.mouseRay
                |> Maybe.andThen
                    (\ray ->
                        List.find (hitTest3d ray 0.1) <| List.reverse game.entities.wheel
                    )

        debugHit =
            case ctx.mouseRay of
                Just ray ->
                    let
                        triangles =
                            toTriangles
                                { position = vec3 0 0 0
                                , rotation = Quaternion.identity
                                , scale = vec3 0.1 0.1 0.1
                                }
                    in
                    List.any (\( a, b, c ) -> hitTest3dTri ray a b c) triangles

                _ ->
                    False

        debugMsg =
            if debugHit then
                log "hit"

            else
                log "miss"

        -- Endgame
        playMsg =
            message
                << Main.RoomMsg
                << Room.ConnectedMsg
                << Connected.GameStateMsg
                << GameState.PlayStateMsg

        { audio } =
            assets

        buttonMsg =
            case state of
                Playing _ ->
                    case Buttons.hit game.buttons pos of
                        Just ( key, _ ) ->
                            case key of
                                "go" ->
                                    playMsg <|
                                        PlayingOnly <|
                                            PlayState.TurnOnly PlayState.EndTurn

                                "goHandFull" ->
                                    playMsg <|
                                        PlayState.PlayingOnly PlayState.IllegalPass

                                "toggleChat" ->
                                    message <|
                                        Main.RoomMsg <|
                                            Room.ConnectedMsg <|
                                                Connected.ChatMsg <|
                                                    Chat.ToggleVisibility

                                _ ->
                                    Cmd.none

                        Nothing ->
                            Cmd.none

                Ended { buttons, replayId } ->
                    case Buttons.hit buttons pos of
                        Just ( key, _ ) ->
                            case key of
                                "playAgain" ->
                                    Cmd.batch
                                        [ playMsg <| PlayState.PlayingOnly PlayState.Continue
                                        , playSound audio "sfx/click.mp3"
                                        ]

                                "watchReplay" ->
                                    case replayId of
                                        Just r ->
                                            Cmd.batch
                                                [ playMsg <| PlayState.GotoReplay r
                                                , playSound audio "sfx/click.mp3"
                                                ]

                                        Nothing ->
                                            Cmd.none

                                "continue" ->
                                    Cmd.batch
                                        [ playMsg <|
                                            PlayState.PlayingOnly PlayState.Continue
                                        , playSound audio "sfx/click.mp3"
                                        ]

                                _ ->
                                    Cmd.none

                        Nothing ->
                            Cmd.none

        endstateMsg =
            case state of
                Ended _ ->
                    if resolving game.res then
                        Cmd.none

                    else
                        message
                            << Main.RoomMsg
                            << Room.ConnectedMsg
                            << Connected.GameStateMsg
                            << GameState.PlayStateMsg
                        <|
                            PlayState.SkipAftermath

                _ ->
                    Cmd.none
    in
    ( newPlayState
    , Cmd.batch
        [ newMsg
        , buttonMsg
        , endstateMsg
        , debugMsg
        ]
    )


mouseUp : Flags -> Assets.Model -> GameType -> Mode -> Players -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseUp _ assets _ mode players _ state =
    let
        game =
            get identity state

        mMsg =
            case game.holding of
                Holding { card, handIndex, pos } ->
                    Just <|
                        PlayingOnly <|
                            TurnOnly <|
                                if Math.Vector3.getY pos > -0.2 then
                                    PlayCard card handIndex pos

                                else
                                    UnholdCard

                NoHolding ->
                    Nothing

        ( newPlayState, cmd ) =
            case mMsg of
                Just msg ->
                    update
                        msg
                        state
                        mode
                        players
                        assets

                Nothing ->
                    ( state, Cmd.none )
    in
    ( newPlayState, cmd )


tutorialAction : Tutorial.Action -> PlayState -> PlayState
tutorialAction action state =
    map
        (\game ->
            { game
                | tutorial = Tutorial.takeAction action game.tutorial
            }
        )
        state
