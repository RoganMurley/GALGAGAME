module GameCommand where

import Card (Card (..))
import CardAnim (CardAnim (..))
import Control.Monad (when)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import DeckBuilding (CharacterChoice, ChosenCharacter (..), DeckBuilding (..), choiceToCharacter, getSelectableRunes, initDeckBuilding, selectCharacter)
import GameState (GameState (..), PlayState (..), PlayingR (..), initModel)
import qualified GodMode
import HandCard (HandCard (..), anyCard)
import Model (Hand, Misc (..), Model (..), Passes (..), Turn)
import Outcome (HoverState (..), Outcome)
import qualified Outcome
import Player (WhichPlayer (..), other)
import qualified Replay.Active as Active
import qualified Replay.Final as Final
import ResolveData (ResolveData (..), resolveAnim)
import Safe (atMay)
import Scenario (Scenario (..))
import Stack (Stack)
import qualified Stack
import StackCard (StackCard (..))
import Stats.Progress (initialProgress)
import StatusEff (applyStatuses)
import User (GameUser (..), User (..), gameusersToUsers, getUser, getUserId, getUsername, isSuperuser, usersToUsernames)
import Util (Err, Gen, deleteIndex, split, times, tupleMap2)
import Wheel (Wheel (..))

data GameCommand
  = EndTurn
  | PlayCard Int
  | HoverCard HoverState
  | Rematch
  | Concede
  | SelectCharacter CharacterChoice
  | Chat Text Text
  | Heartbeat
  | God Text
  deriving (Show)

update :: GameCommand -> WhichPlayer -> GameState -> Scenario -> (Maybe GameUser, Maybe GameUser) -> UTCTime -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ _ _ _ = chat username msg
update cmd which state scenario users time =
  case state of
    Waiting _ _ ->
      case cmd of
        Heartbeat ->
          ignore
        _ ->
          Left ("Unknown command " <> cs (show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter character ->
          select which character selectModel turn scenario gen users time
        Heartbeat ->
          ignore
        _ ->
          Left ("Unknown command " <> cs (show cmd) <> " on a selecting GameState")
    Started playState ->
      case playState of
        Playing playing ->
          case cmd of
            EndTurn ->
              endTurn which playing scenario time
            PlayCard index ->
              playCard index which playing scenario time
            HoverCard hover ->
              hoverCard hover which playing
            Concede ->
              concede which state []
            Heartbeat ->
              heartbeat time playing
            God str ->
              godMode (getUser which users) str which time playing
            _ ->
              Left ("Unknown command " <> cs (show cmd) <> " on a Playing GameState")
        Ended winner _ _ gen ->
          case cmd of
            Rematch ->
              rematch (winner, gen) users scenario time
            HoverCard _ ->
              ignore
            Heartbeat ->
              ignore
            _ ->
              Left ("Unknown command " <> cs (show cmd) <> " on an Ended GameState")

ignore :: Either Err (Maybe GameState, [Outcome])
ignore = Right (Nothing, [])

rematch ::
  (Maybe WhichPlayer, Gen) ->
  (Maybe GameUser, Maybe GameUser) ->
  Scenario ->
  UTCTime ->
  Either Err (Maybe GameState, [Outcome])
rematch (winner, gen) gameusers scenario time =
  let users = gameusersToUsers gameusers
      (userPa, userPb) = users
      superPa = maybe False isSuperuser userPa
      superPb = maybe False isSuperuser userPb
      deckModel =
        initDeckBuilding
          (superPa, superPb)
          (scenario_characterPa scenario)
          (scenario_characterPb scenario)
      turn = fromMaybe PlayerA winner
      startProgram = scenario_prog scenario
      timeLimit = scenario_timeLimit scenario
      (newState, outcomes) =
        nextSelectState
          deckModel
          turn
          (startProgram (usersToUsernames users))
          (fst $ split gen)
          (userPa, userPb)
          time
          timeLimit
   in Right (Just newState, outcomes)

chat :: Text -> Text -> Either Err (Maybe GameState, [Outcome])
chat username msg =
  Right
    ( Nothing,
      [Outcome.Encodable $ Outcome.Chat username msg]
    )

concede :: WhichPlayer -> GameState -> [ResolveData] -> Either Err (Maybe GameState, [Outcome])
concede which (Started (Playing playing)) extraRes =
  let model = playing_model playing :: Model
      replay = playing_replay playing :: Active.Replay
      gen = Alpha.evalI model Alpha.getGen :: Gen
      winner = Just $ other which :: Maybe WhichPlayer
      res = extraRes ++ [resolveAnim $ GameEnd winner] :: [ResolveData]
      newReplay = replay `Active.add` res :: Active.Replay
      newPlayState = Ended winner model newReplay gen :: PlayState
      finalReplay = Final.finalise newReplay newPlayState :: Final.Replay
   in Right
        ( Just . Started $ newPlayState,
          [ Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing,
            Outcome.HandleProgress winner,
            Outcome.SaveReplay finalReplay
          ]
        )
concede _ _ _ =
  Left "Cannot concede when not playing"

select ::
  WhichPlayer ->
  CharacterChoice ->
  DeckBuilding ->
  Turn ->
  Scenario ->
  Gen ->
  (Maybe GameUser, Maybe GameUser) ->
  UTCTime ->
  Either Err (Maybe GameState, [Outcome])
select which choice deckModel turn scenario gen gameusers time =
  let users = gameusersToUsers gameusers
      user = getUser which gameusers
      progress = maybe initialProgress gameuser_progress user
      runes = getSelectableRunes which deckModel
   in case choiceToCharacter choice runes progress of
        Right character ->
          let newDeckModel :: DeckBuilding
              newDeckModel = selectCharacter deckModel which character
              startProgram = scenario_prog scenario
              timeLimit = scenario_timeLimit scenario
              (newState, outcomes) =
                nextSelectState
                  newDeckModel
                  turn
                  (startProgram (usersToUsernames users))
                  gen
                  users
                  time
                  timeLimit
           in Right (Just newState, outcomes)
        Left err ->
          Left err

nextSelectState ::
  DeckBuilding ->
  Turn ->
  Beta.Program () ->
  Gen ->
  (Maybe User, Maybe User) ->
  UTCTime ->
  NominalDiffTime ->
  (GameState, [Outcome])
nextSelectState deckModel turn startProgram gen (mUserPa, mUserPb) time timeLimit =
  let result :: Maybe ([ResolveData], Model, PlayState)
      result =
        case deckModel of
          DeckBuilding (Right (ChosenCharacter ca)) _ (Right (ChosenCharacter cb)) _ ->
            let model = initModel turn ca cb gen :: Model
                displayUsernamePa = maybe "" getUsername mUserPa :: Text
                displayUsernamePb = maybe "" getUsername mUserPb :: Text
                userIdPa = getUserId =<< mUserPa :: Maybe Int64
                userIdPb = getUserId =<< mUserPb :: Maybe Int64
                replayUserPa = (displayUsernamePa, userIdPa)
                replayUserPb = (displayUsernamePb, userIdPb)
                replay = Active.init model replayUserPa replayUserPb :: Active.Replay
                (newModel, res) = Beta.execute model $ Beta.betaI startProgram
                playstate :: PlayState
                playstate =
                  Playing $
                    PlayingR
                      { playing_model = newModel,
                        playing_replay = replay `Active.add` res,
                        playing_utc = Just time,
                        playing_timeLimit = timeLimit
                      }
             in Just (res, model, playstate)
          _ ->
            Nothing
      state :: GameState
      state =
        case result of
          Nothing ->
            Selecting deckModel turn gen
          Just (_, _, playstate) ->
            Started playstate
      outcomes :: [Outcome]
      outcomes =
        case result of
          Nothing ->
            [Outcome.Sync]
          Just (res, model, playstate) ->
            [Outcome.Encodable $ Outcome.Resolve res model playstate Nothing]
   in (state, outcomes)

playCard :: Int -> WhichPlayer -> PlayingR -> Scenario -> UTCTime -> Either Err (Maybe GameState, [Outcome])
playCard index which playing scenario time
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise =
    case card of
      Nothing ->
        Left . cs $ "You can't play a card you don't have in your hand (" ++ show index ++ "," ++ show which ++ ")"
      Just c ->
        let program :: Beta.Program ()
            program = do
              Beta.play which c index
              Beta.windup
              Beta.raw $ Alpha.setHold True
            (modelA, resA) = Beta.execute model $ Beta.betaI program
            (result, resB) =
              runWriter $
                resolveAll
                  playing
                    { playing_model = modelA,
                      playing_replay = replay `Active.add` resA
                    }
            isWheelFull :: Bool
            isWheelFull = Alpha.evalI modelA $ do
              stack <- Alpha.getStack
              return $ foldr (&&) True (isJust <$> stack)
         in case result of
              Playing newPlaying ->
                let modelB = playing_model newPlaying
                    -- If the wheel is full, end the round.
                    postProgram = when isWheelFull (scenario_roundEndProg scenario)
                    (modelC, resC) = Beta.execute modelB $ Beta.betaI postProgram
                    newPlayState :: PlayState
                    newPlayState =
                      checkWin $
                        newPlaying
                          { playing_model = modelC,
                            playing_replay = playing_replay newPlaying `Active.add` resC,
                            playing_utc = Just time
                          }
                 in -- The player who played the card does that animation clientside, so don't send resA.
                    Right
                      ( Just . Started $ newPlayState,
                        [ Outcome.Encodable $ Outcome.Resolve (resA ++ resB ++ resC) model newPlayState (Just which),
                          Outcome.Encodable $ Outcome.Resolve (resB ++ resC) modelA newPlayState (Just (other which))
                        ]
                      )
              Ended w m newReplay g ->
                let newPlayState = Ended w m newReplay g :: PlayState
                    newState = Started newPlayState :: GameState
                    finalReplay = Final.finalise newReplay newPlayState :: Final.Replay
                 in Right
                      ( Just newState,
                        [ Outcome.Encodable $ Outcome.Resolve (resA ++ resB) model newPlayState (Just which),
                          Outcome.Encodable $ Outcome.Resolve resB modelA newPlayState (Just (other which)),
                          Outcome.HandleProgress w,
                          Outcome.SaveReplay finalReplay
                        ]
                      )
  where
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
    (hand, turn, card) =
      Alpha.evalI model $ do
        h <- Alpha.getHand which
        t <- Alpha.getTurn
        let c = atMay hand index :: Maybe HandCard
        return (h, t, c)

endTurn :: WhichPlayer -> PlayingR -> Scenario -> UTCTime -> Either Err (Maybe GameState, [Outcome])
endTurn which playing scenario time
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | full = Left "You can't end the turn when your hand is full"
  | otherwise =
    case passes of
      OnePass ->
        case runWriter $ resolveAll playing of
          (Playing newPlaying, res) ->
            let (newModel, endRes) = Beta.execute (playing_model newPlaying) $ Beta.betaI (scenario_roundEndProg scenario)
                newPlayState :: PlayState
                newPlayState =
                  checkWin $
                    newPlaying
                      { playing_model = newModel,
                        playing_replay = replay `Active.add` res `Active.add` endRes,
                        playing_utc = Just time
                      }
                newState = Started newPlayState :: GameState
             in Right
                  ( Just newState,
                    [Outcome.Encodable $ Outcome.Resolve (res ++ endRes) model newPlayState Nothing]
                  )
          (Ended w m newReplay g, res) ->
            let newPlayState = Ended w m newReplay g :: PlayState
                newState = Started newPlayState :: GameState
                finalReplay = Final.finalise newReplay newPlayState :: Final.Replay
             in Right
                  ( Just newState,
                    [ Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing,
                      Outcome.HandleProgress w,
                      Outcome.SaveReplay finalReplay
                    ]
                  )
      NoPass ->
        let endTurnProgram :: Beta.Program ()
            endTurnProgram = do
              Beta.raw Alpha.swapTurn
              Beta.rawAnim $ Pass which
            (newModel, res) = Beta.execute model $ Beta.betaI endTurnProgram
            newPlayState :: PlayState
            newPlayState =
              Playing $
                playing
                  { playing_model = newModel,
                    playing_replay = replay `Active.add` res,
                    playing_utc = Just time
                  }
         in Right
              ( Just . Started $ newPlayState,
                [Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing]
              )
  where
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
    (turn, passes) =
      Alpha.evalI model $ do
        t <- Alpha.getTurn
        p <- Alpha.getPasses
        return (t, p)
    full = Alpha.evalI model $ Alpha.handFull which :: Bool

resolveAll :: PlayingR -> Writer [ResolveData] PlayState
resolveAll playing = resolveAll' playing 0 id

resolveAll' :: PlayingR -> Int -> (Card -> Card) -> Writer [ResolveData] PlayState
resolveAll' playing resolutionCount rewrite = do
  let model = playing_model playing
  let replay = playing_replay playing
  let (modelA, resA) = Beta.execute model preProgram :: (Model, [ResolveData])
  tell resA
  let activeCard = Alpha.evalI modelA (wheel_0 <$> Alpha.getStack)
  case activeCard of
    Just stackCard -> do
      let (modelB, resB) = Beta.execute modelA (cardProgram stackCard) :: (Model, [ResolveData])
      tell resB
      let newReplay = replay `Active.add` resA `Active.add` resB
      case checkWin (playing {playing_model = modelB, playing_replay = newReplay}) of
        Playing newPlaying ->
          resolveAll' newPlaying nextResolutionCount rewrite
        Ended w m finalReplay gen -> do
          let endRes = [resolveAnim $ GameEnd w]
          tell endRes
          return $ Ended w m (finalReplay `Active.add` endRes) gen
    Nothing ->
      return . Playing $
        playing
          { playing_model = modelA,
            playing_replay = replay `Active.add` resA
          }
  where
    isFinite :: Bool
    isFinite = resolutionCount < 20
    nextResolutionCount :: Int
    nextResolutionCount = if isFinite then resolutionCount + 1 else 0
    preProgram :: Beta.ExecProgram ()
    preProgram =
      Beta.betaI $ do
        holding <- Beta.getHold
        when (not holding) Beta.rotate
        Beta.raw (Alpha.setHold False)
        Beta.refreshGen
    cardProgram :: StackCard -> Beta.ExecProgram ()
    cardProgram StackCard {stackcard_card, stackcard_owner} =
      Beta.betaI $ do
        let eff = card_eff (rewrite . applyStatuses $ stackcard_card)
        when isFinite $ eff stackcard_owner
        holding <- Beta.getHold
        when (not holding) $ Beta.raw $ Alpha.modStack (\wheel -> wheel {wheel_0 = Nothing})

checkWin :: PlayingR -> PlayState
checkWin playing
  | isJust forceWin =
    Ended forceWin model replay gen
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing model replay gen
  | lifePB <= 0 =
    Ended (Just PlayerA) model replay gen
  | lifePA <= 0 =
    Ended (Just PlayerB) model replay gen
  | otherwise =
    Playing playing
  where
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
    (gen, lifePA, lifePB, forceWin) =
      Alpha.evalI model $ do
        g <- Alpha.getGen
        la <- Alpha.getLife PlayerA
        lb <- Alpha.getLife PlayerB
        fw <- misc_forceWin <$> Alpha.getMisc
        return (g, la, lb, fw)

hoverCard :: HoverState -> WhichPlayer -> PlayingR -> Either Err (Maybe GameState, [Outcome])
hoverCard (HoverHand i) which playing =
  let hand = Alpha.evalI model $ Alpha.getHand which :: Hand
      model = playing_model playing :: Model
   in case atMay hand i of
        Just handCard ->
          Right (Nothing, [Outcome.Encodable $ Outcome.Hover which (HoverHand i) hoverDamage])
          where
            card = anyCard handCard
            newModel = Alpha.modI model $ do
              Alpha.modHand which (deleteIndex i)
              Alpha.modStack (\s -> (Stack.windup s) {wheel_0 = Just $ StackCard which card})
            damage = Beta.damageNumbersI newModel $ card_eff (applyStatuses card) which
            hoverDamage = tupleMap2 Outcome.damageToHoverDamage damage
        Nothing ->
          ignore
hoverCard (HoverOtherHand i) which playing =
  let hand = Alpha.evalI model $ Alpha.getHand (other which) :: Hand
      model = playing_model playing :: Model
   in case atMay hand i of
        Just (KnownHandCard card) ->
          Right (Nothing, [Outcome.Encodable $ Outcome.Hover which (HoverOtherHand i) hoverDamage])
          where
            newModel = Alpha.modI model $ do
              Alpha.modHand (other which) (deleteIndex i)
              Alpha.modStack (\s -> (Stack.windup s) {wheel_0 = Just $ StackCard (other which) card})
            damage = Beta.damageNumbersI newModel $ card_eff (applyStatuses card) (other which)
            hoverDamage = tupleMap2 Outcome.damageToHoverDamage damage
        _ ->
          ignore
hoverCard (HoverStack i) which playing =
  let stack = Alpha.evalI model $ Alpha.getStack :: Stack
      model = playing_model playing :: Model
   in case atMay (toList stack) i of
        Just (Just (StackCard owner card)) ->
          Right (Nothing, [Outcome.Encodable $ Outcome.Hover which (HoverStack i) hoverDamage])
          where
            newModel = Alpha.modI model $ do
              Alpha.modStack $ times i (\s -> Stack.rotate (s {wheel_0 = Nothing}))
              Alpha.modRot ((-) i)
            damage = Beta.damageNumbersI newModel $ card_eff (applyStatuses card) owner
            hoverDamage = tupleMap2 Outcome.damageToHoverDamage damage
        _ ->
          ignore
hoverCard NoHover which _ =
  let hoverDamage = tupleMap2 Outcome.damageToHoverDamage (0, 0)
   in Right (Nothing, [Outcome.Encodable $ Outcome.Hover which NoHover hoverDamage])

godMode :: Maybe GameUser -> Text -> WhichPlayer -> UTCTime -> PlayingR -> Either Err (Maybe GameState, [Outcome])
godMode mUser str which time playing =
  let model = playing_model playing :: Model
      replay = playing_replay playing :: Active.Replay
   in if maybe False (isSuperuser . gameuser_user) mUser
        then case GodMode.parse which str of
          GodMode.ParsedProgram betaProgram ->
            let program = Beta.betaI betaProgram :: Beta.ExecProgram ()
                (m, res) = Beta.execute model program :: (Model, [ResolveData])
                newPlayState =
                  checkWin $
                    playing
                      { playing_model = m,
                        playing_replay = replay `Active.add` res
                      }
             in Right
                  ( Just . Started $ newPlayState,
                    [Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing]
                  )
          GodMode.ParsedTimeLimit timeLimit ->
            Right
              ( Just . Started . Playing $
                  playing
                    { playing_utc = Just time,
                      playing_timeLimit = fromIntegral timeLimit
                    },
                [Outcome.Encodable $ Outcome.Heartbeat $ fromIntegral timeLimit]
              )
          GodMode.ParseError err ->
            Left err
        else Left "Not a superuser"

heartbeat :: UTCTime -> PlayingR -> Either Err (Maybe GameState, [Outcome])
heartbeat currentTime playing =
  let previousTime = fromMaybe currentTime $ playing_utc playing :: UTCTime
      model = playing_model playing :: Model
      timeLimit = playing_timeLimit playing :: NominalDiffTime
      delta = diffUTCTime currentTime previousTime :: NominalDiffTime
      which = model_turn model :: WhichPlayer
      timeLeft = timeLimit - delta :: NominalDiffTime
      (_, res) = Beta.execute model $ Beta.betaI (Beta.rawAnim Timeout)
   in if delta > timeLimit
        then concede which (Started . Playing $ playing) res
        else Right (Nothing, [Outcome.Encodable $ Outcome.Heartbeat timeLeft])
