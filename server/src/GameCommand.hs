module GameCommand where

import Card (Card(..))
import CardAnim (CardAnim(..))
import Control.Monad (join, when)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import DeckBuilding (CharacterChoice, ChosenCharacter(..), DeckBuilding(..), choiceToCharacter, initDeckBuilding, selectCharacter)
import GameState (GameState(..), PlayState(..), PlayingR(..), initModel)
import Model (Hand, Passes(..), Model(..), Turn)
import Outcome (HoverState(..), Outcome)
import Player (WhichPlayer(..), other)
import ResolveData (ResolveData(..), resolveAnim)
import Safe (atMay)
import Scenario (Scenario(..))
import Stack (Stack)
import StackCard (StackCard(..))
import StatusEff (applyStatuses)
import User (User(..), getUsername, getQueryUsername, isSuperuser)
import Util (Err, Gen, deleteIndex, split, times, tupleMap2)
import Wheel (Wheel(..))


import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import qualified GodMode
import qualified Outcome
import qualified Replay.Active as Active
import qualified Replay.Final as Final
import qualified Stack


data GameCommand =
    EndTurn
  | PlayCard Int
  | HoverCard HoverState
  | Rematch
  | Concede
  | SelectCharacter CharacterChoice
  | Chat Text Text
  | Heartbeat
  | God Text
  deriving (Show)


update :: GameCommand -> WhichPlayer -> GameState -> Scenario -> (Maybe User, Maybe User) -> UTCTime -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ _ _ _      = chat username msg
update cmd which state scenario users time =
  case state of
    Waiting _ _ ->
      case cmd of
        Heartbeat ->
          ignore
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter character ->
          select which character selectModel turn scenario gen users time
        Heartbeat ->
          ignore
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a selecting GameState")
    Started playState ->
      case playState of
        Playing playing ->
          case cmd of
            EndTurn ->
              endTurn which playing time
            PlayCard index ->
              playCard index which playing time
            HoverCard hover ->
              hoverCard hover which playing
            Concede ->
              concede which state
            Heartbeat ->
              heartbeat time playing
            God str ->
              godMode (getUser which users) str which time playing
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner _ _ gen ->
          case cmd of
            Rematch ->
              rematch (winner, gen) users scenario time
            HoverCard _ ->
              ignore
            Heartbeat ->
              ignore
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


ignore :: Either Err (Maybe GameState, [Outcome])
ignore = Right (Nothing, [])


rematch :: (Maybe WhichPlayer, Gen)
        -> (Maybe User, Maybe User)
        -> Scenario
        -> UTCTime
        -> Either Err (Maybe GameState, [Outcome])
rematch (winner, gen) users scenario time =
  let
    deckModel = initDeckBuilding (scenario_characterPa scenario) (scenario_characterPb scenario)
    turn = fromMaybe PlayerA winner
    startProgram = scenario_prog scenario
    timeLimit = scenario_timeLimit scenario
    (newState, outcomes) = nextSelectState deckModel turn startProgram (fst $ split gen) users time timeLimit
  in
    Right (Just newState, outcomes)


chat :: Text -> Text -> Either Err (Maybe GameState, [Outcome])
chat username msg =
  Right (
    Nothing
  , [ Outcome.Encodable $ Outcome.Chat username msg ]
  )


concede :: WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
concede which (Started (Playing playing)) =
  let
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
    gen = Alpha.evalI model Alpha.getGen :: Gen
    winner = Just $ other which :: Maybe WhichPlayer
    res = [resolveAnim $ GameEnd winner] :: [ResolveData]
    newReplay = Active.add replay res :: Active.Replay
    newPlayState = Ended winner model newReplay gen :: PlayState
    finalReplay = Final.finalise newReplay newPlayState :: Final.Replay
  in
    Right (
      Just . Started $ newPlayState,
      [ Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing
      , Outcome.HandleExperience winner
      , Outcome.SaveReplay finalReplay
      ]
    )
concede _ _ =
  Left "Cannot concede when not playing"


select :: WhichPlayer
       -> CharacterChoice
       -> DeckBuilding
       -> Turn
       -> Scenario
       -> Gen
       -> (Maybe User, Maybe User)
       -> UTCTime
       -> Either Err (Maybe GameState, [Outcome])
select which choice deckModel turn scenario gen users time =
  case choiceToCharacter choice of
    Right character ->
      let
        newDeckModel :: DeckBuilding
        newDeckModel = selectCharacter deckModel which character
        startProgram = scenario_prog scenario
        timeLimit = scenario_timeLimit scenario
        (newState, outcomes) = nextSelectState newDeckModel turn startProgram gen users time timeLimit
      in
        Right (Just newState, outcomes)
    Left err ->
      Left err


nextSelectState :: DeckBuilding
                -> Turn
                -> Beta.Program ()
                -> Gen
                -> (Maybe User, Maybe User)
                -> UTCTime
                -> NominalDiffTime
                -> (GameState, [Outcome])
nextSelectState deckModel turn startProgram gen (mUserPa, mUserPb) time timeLimit =
  let
    result :: Maybe ([ResolveData], Model, PlayState)
    result =
      case deckModel of
        DeckBuilding (Right (ChosenCharacter ca)) (Right (ChosenCharacter cb)) ->
          let
            model = initModel turn ca cb gen :: Model
            displayUsernamePa = fromMaybe "" $ getUsername <$> mUserPa :: Text
            displayUsernamePb = fromMaybe "" $ getUsername <$> mUserPb :: Text
            queryUsernamePa = fromMaybe "" $ join $ getQueryUsername <$> mUserPa :: Text
            queryUsernamePb = fromMaybe "" $ join $ getQueryUsername <$> mUserPb :: Text
            usernamesPa = (displayUsernamePa, queryUsernamePa)
            usernamesPb = (displayUsernamePb, queryUsernamePb)
            replay = Active.init model usernamesPa usernamesPb :: Active.Replay
            (newModel, _, res) = Beta.execute model $ foldFree Beta.betaI startProgram
            playstate :: PlayState
            playstate = Playing $ PlayingR
              { playing_model = newModel
              , playing_replay = Active.add replay res
              , playing_utc = Just time
              , playing_timeLimit = timeLimit
              }
          in
            Just (res, model, playstate)
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
          [ Outcome.Sync ]
        Just (res, model, playstate) ->
          [ Outcome.Encodable $ Outcome.Resolve res model playstate Nothing ]
  in
    (state, outcomes)


playCard :: Int -> WhichPlayer -> PlayingR -> UTCTime -> Either Err (Maybe GameState, [Outcome])
playCard index which playing time
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise     =
    case card of
      Nothing ->
        Left . cs $ "You can't play a card you don't have in your hand (" ++ show index ++ "," ++ show which ++ ")"
      Just c ->
        let
          program :: Beta.Program ()
          program = do
            Beta.play which c index
            Beta.windup
            Beta.raw $ Alpha.setHold True
          (newModel, _, res) = Beta.execute model $ foldFree Beta.betaI program
          (result, newRes) = runWriter $ resolveAll playing
            { playing_model = newModel
            , playing_replay = replay `Active.add` res
            }
        in
          case result of
            Playing newPlaying ->
              let
                newPlayState :: PlayState
                newPlayState = Playing $ newPlaying
                  { playing_model = playing_model newPlaying
                  , playing_replay = playing_replay newPlaying `Active.add` newRes
                  , playing_utc = Just time
                  }
              in
              -- The player who played the card does that animation clientside, so only send the rest.
              Right (
                Just . Started $ newPlayState,
                [ Outcome.Encodable $ Outcome.Resolve (res ++ newRes) model    newPlayState (Just which)
                , Outcome.Encodable $ Outcome.Resolve newRes          newModel newPlayState (Just (other which))
                ]
              )
            Ended w m newReplay g ->
              let
                newPlayState = Ended w m newReplay g :: PlayState
                newState     = Started newPlayState :: GameState
                finalReplay  = Final.finalise newReplay newPlayState :: Final.Replay
              in
                Right (
                  Just newState,
                  [ Outcome.Encodable $ Outcome.Resolve (res ++ newRes) model    newPlayState (Just which)
                  , Outcome.Encodable $ Outcome.Resolve newRes          newModel newPlayState (Just (other which))
                  , Outcome.HandleExperience w
                  , Outcome.SaveReplay finalReplay
                  ]
                )
  where
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
    (hand, turn, card) =
      Alpha.evalI model $ do
        h <- Alpha.getHand which
        t <- Alpha.getTurn
        let c = atMay hand index :: Maybe Card
        return (h, t, c)


endTurn :: WhichPlayer -> PlayingR -> UTCTime -> Either Err (Maybe GameState, [Outcome])
endTurn which playing time
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | full          = Left "You can't end the turn when your hand is full"
  | otherwise     =
    case passes of
      OnePass ->
        case runWriter $ resolveAll playing of
          (Playing newPlaying, res) ->
            let
              roundEndProgram :: Beta.Program ()
              roundEndProgram = do
                Beta.raw Alpha.swapTurn
                Beta.raw Alpha.resetPasses
                Beta.draw PlayerA PlayerA 1
                Beta.draw PlayerB PlayerB 1
              (newModel, _, endRes) = Beta.execute (playing_model newPlaying) $ foldFree Beta.betaI roundEndProgram
              newPlayState :: PlayState
              newPlayState = Playing $ newPlaying
                { playing_model = newModel
                , playing_replay = replay `Active.add` endRes
                , playing_utc = Just time
                }
              newState = Started newPlayState :: GameState
            in
              Right (
                Just newState,
                [ Outcome.Encodable $ Outcome.Resolve (res ++ endRes) model newPlayState Nothing ]
              )
          (Ended w m newReplay g, res) ->
            let
              newPlayState = Ended w m newReplay g :: PlayState
              newState     = Started newPlayState  :: GameState
              finalReplay = Final.finalise newReplay newPlayState :: Final.Replay
            in
              Right (
                Just newState,
                [ Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing
                , Outcome.HandleExperience w
                , Outcome.SaveReplay finalReplay
                ]
              )
      NoPass ->
        let
          endTurnProgram :: Beta.Program ()
          endTurnProgram = do
            Beta.raw Alpha.swapTurn
            Beta.rawAnim $ Pass which
          (newModel, _, res) = Beta.execute model $ foldFree Beta.betaI endTurnProgram
          newPlayState :: PlayState
          newPlayState = Playing $ playing
            { playing_model = newModel
            , playing_replay = replay `Active.add` res
            , playing_utc = Just time
            }
        in
          Right (
            Just . Started $ newPlayState,
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
  let (modelA, _, resA) = Beta.execute model preProgram :: (Model, String, [ResolveData])
  tell resA
  let mStackCard = Alpha.evalI modelA (wheel_0 <$> Alpha.getStack)
  case mStackCard of
    Just stackCard -> do
      let (modelB, _, resB) = Beta.execute modelA (cardProgram stackCard) :: (Model, String, [ResolveData])
      tell resB
      let newReplay = replay `Active.add` resA `Active.add` resB
      case checkWin (playing { playing_model = modelB, playing_replay = newReplay }) of
        Playing newPlaying ->
          resolveAll' newPlaying nextResolutionCount rewrite
        Ended w m finalReplay gen -> do
          let endRes = [resolveAnim $ GameEnd w]
          tell endRes
          return (Ended w m (finalReplay `Active.add` endRes) gen)
    Nothing ->
      return . Playing $ playing
        { playing_model = modelA
        , playing_replay = replay `Active.add` resA
        }
  where
    isFinite :: Bool
    isFinite = resolutionCount < 20
    nextResolutionCount :: Int
    nextResolutionCount = if isFinite then resolutionCount + 1 else 0
    preProgram :: Beta.AlphaLogAnimProgram ()
    preProgram =
      foldFree Beta.betaI $ do
        holding <- Beta.getHold
        when (not holding) Beta.rotate
        Beta.raw (Alpha.setHold False)
        Beta.refreshGen
    cardProgram :: StackCard -> Beta.AlphaLogAnimProgram ()
    cardProgram StackCard{ stackcard_card, stackcard_owner } =
      foldFree Beta.betaI $ do
        let eff = card_eff (rewrite . applyStatuses $ stackcard_card)
        when isFinite $ eff stackcard_owner
        holding <- Beta.getHold
        when (not holding) $ Beta.raw $ Alpha.modStack (\wheel -> wheel { wheel_0 = Nothing })


checkWin :: PlayingR -> PlayState
checkWin playing
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
    (gen, lifePA, lifePB) =
      Alpha.evalI model $ do
        g  <- Alpha.getGen
        la <- Alpha.getLife PlayerA
        lb <- Alpha.getLife PlayerB
        return (g, la, lb)


hoverCard :: HoverState -> WhichPlayer -> PlayingR -> Either Err (Maybe GameState, [Outcome])
hoverCard (HoverHand i) which playing =
  let
    hand = Alpha.evalI model $ Alpha.getHand which :: Hand
    model = playing_model playing :: Model
  in
    case atMay hand i of
      Just card ->
        Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (HoverHand i) hoverDamage ])
        where
          newModel = Alpha.modI model $ do
            Alpha.modHand which (deleteIndex i)
            Alpha.modStack (\s -> (Stack.windup s) { wheel_0 = Just $ StackCard which card })
          damage = Beta.damageNumbersI newModel $ card_eff (applyStatuses card) which
          hoverDamage = tupleMap2 Outcome.damageToHoverDamage damage
      Nothing ->
        ignore
hoverCard (HoverStack i) which playing =
  let
    stack = Alpha.evalI model $ Alpha.getStack :: Stack
    model = playing_model playing :: Model
  in
    case atMay (toList stack) i of
      Just (Just (StackCard owner card)) ->
        Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (HoverStack i) hoverDamage ])
        where
          newModel = Alpha.modI model $ do
            Alpha.modStack $ times i (\s -> Stack.rotate (s { wheel_0 = Nothing }))
            Alpha.modRot ((-) i)
          damage = Beta.damageNumbersI newModel $ card_eff (applyStatuses card) owner
          hoverDamage = tupleMap2 Outcome.damageToHoverDamage damage
      _ ->
        ignore
hoverCard NoHover which _ =
  let
    hoverDamage = tupleMap2 Outcome.damageToHoverDamage (0, 0)
  in
    Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which NoHover hoverDamage ])


godMode :: Maybe User -> Text -> WhichPlayer -> UTCTime -> PlayingR -> Either Err (Maybe GameState, [Outcome])
godMode mUser str which time playing =
  let
    model = playing_model playing :: Model
    replay = playing_replay playing :: Active.Replay
  in
    if fromMaybe False $ isSuperuser <$> mUser then
      case GodMode.parse which str of
        GodMode.ParsedProgram betaProgram ->
          let
            program = foldFree Beta.betaI $ betaProgram :: Beta.AlphaLogAnimProgram ()
            (m, _, res) = Beta.execute model program :: (Model, String, [ResolveData])
            newPlayState = Playing $ playing
              { playing_model = m
              , playing_replay = Active.add replay res
              }
          in
            Right (
              Just . Started $ newPlayState
            , [Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing]
            )
        GodMode.ParsedTimeLimit timeLimit ->
          Right (
            Just . Started . Playing $ playing
              { playing_utc = Just time
              , playing_timeLimit = fromIntegral timeLimit
              }
          , [Outcome.Encodable $ Outcome.Heartbeat $ fromIntegral timeLimit]
          )
        GodMode.ParseError err ->
          Left err
    else
      Left "Not a superuser"


heartbeat :: UTCTime -> PlayingR -> Either Err (Maybe GameState, [Outcome])
heartbeat currentTime playing =
  let
    previousTime = fromMaybe currentTime $ playing_utc playing :: UTCTime
    model = playing_model playing :: Model
    timeLimit = playing_timeLimit playing :: NominalDiffTime
    delta = diffUTCTime currentTime previousTime :: NominalDiffTime
    which = model_turn model :: WhichPlayer
    timeLeft = timeLimit - delta :: NominalDiffTime
  in
    if delta > timeLimit then
      concede which (Started . Playing $ playing)
        else Right (Nothing, [Outcome.Encodable $ Outcome.Heartbeat timeLeft])


getUser :: WhichPlayer -> (Maybe User, Maybe User) -> Maybe User
getUser PlayerA (ua, _) = ua
getUser PlayerB (_, ub) = ub
