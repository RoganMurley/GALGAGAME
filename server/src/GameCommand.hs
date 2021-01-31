module GameCommand where

import Card (Card(..))
import CardAnim (CardAnim(..))
import Control.Monad (join, when)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import DeckBuilding (CharacterChoice, DeckBuilding(..), choiceToCharacter, initDeckBuilding, selectCharacter)
import GameState (GameState(..), PlayState(..), initModel)
import Model (Hand, Passes(..), Model, Turn)
import Outcome (HoverState(..), Outcome)
import Player (WhichPlayer(..), other)
import ResolveData (ResolveData(..), resolveAnim)
import Safe (atMay)
import Scenario (Scenario(..))
import Stack (Stack)
import StackCard (StackCard(..))
import User (User(..), getUsername, getQueryUsername, isSuperuser)
import Util (Err, Gen, deleteIndex, split, times)
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
  | God Text
  deriving (Show)


update :: GameCommand -> WhichPlayer -> GameState -> Scenario -> (Maybe User, Maybe User) -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ _ _        = chat username msg
update cmd which state scenario users =
  case state of
    Waiting _ _ ->
      Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter character ->
          select which character selectModel turn scenario gen users
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a selecting GameState")
    Started playState ->
      case playState of
        Playing model replay ->
          case cmd of
            EndTurn ->
              endTurn which model replay
            PlayCard index ->
              playCard index which model replay
            HoverCard hover ->
              hoverCard hover which model
            Concede ->
              concede which state
            God str ->
              godMode (getUser which users) str which model replay
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner _ _ gen ->
          case cmd of
            Rematch ->
              rematch (winner, gen) users scenario
            HoverCard _ ->
              ignore
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


ignore :: Either Err (Maybe GameState, [Outcome])
ignore = Right (Nothing, [])


rematch :: (Maybe WhichPlayer, Gen) -> (Maybe User, Maybe User) -> Scenario -> Either Err (Maybe GameState, [Outcome])
rematch (winner, gen) users scenario =
  let
    deckModel = initDeckBuilding (scenario_characterPa scenario) (scenario_characterPb scenario)
    turn = fromMaybe PlayerA winner
    startProgram = scenario_prog scenario
    (newState, outcomes) = nextSelectState deckModel turn startProgram (fst $ split gen) users
  in
    Right (Just newState, outcomes)


chat :: Text -> Text -> Either Err (Maybe GameState, [Outcome])
chat username msg =
  Right (
    Nothing
  , [ Outcome.Encodable $ Outcome.Chat username msg ]
  )


concede :: WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
concede which (Started (Playing model replay)) =
  let
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


select :: WhichPlayer -> CharacterChoice -> DeckBuilding -> Turn -> Scenario -> Gen -> (Maybe User, Maybe User) -> Either Err (Maybe GameState, [Outcome])
select which choice deckModel turn scenario gen users =
  case choiceToCharacter choice of
    Right character ->
      let
        newDeckModel :: DeckBuilding
        newDeckModel = selectCharacter deckModel which character
        startProgram = scenario_prog scenario
        (newState, outcomes) = nextSelectState newDeckModel turn startProgram gen users
      in
        Right (Just newState, outcomes)
    Left err ->
      Left err


nextSelectState :: DeckBuilding -> Turn -> Beta.Program () -> Gen -> (Maybe User, Maybe User) -> (GameState, [Outcome])
nextSelectState deckModel turn startProgram gen (mUserPa, mUserPb) =
  let
    result :: Maybe ([ResolveData], Model, PlayState)
    result =
      case deckModel of
        (DeckBuilding (Just ca) (Just cb)) ->
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
            playstate = Playing newModel (Active.add replay res)
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


playCard :: Int -> WhichPlayer -> Model -> Active.Replay -> Either Err (Maybe GameState, [Outcome])
playCard index which model replay
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise     =
    case card of
      Nothing ->
        Left "You can't play a card you don't have in your hand"
      Just c ->
        let
          program :: Beta.Program ()
          program = do
            Beta.play which c index
            Beta.windup
            Beta.raw $ Alpha.setHold True
          (newModel, _, res) = Beta.execute model $ foldFree Beta.betaI program
        in
          case runWriter $ resolveAll newModel (replay `Active.add` res) 0 of
            (Playing m newReplay, newRes) ->
              let
                newPlayState = Playing m (newReplay `Active.add` newRes)
                -- The player who played the card does that animation clientside, so only send the rest.
              in
                Right (
                  Just . Started $ newPlayState,
                  [ Outcome.Encodable $ Outcome.Resolve (res ++ newRes) model    newPlayState (Just which)
                  , Outcome.Encodable $ Outcome.Resolve newRes          newModel newPlayState (Just (other which))
                  ]
                )
            (Ended w m newReplay g, newRes) ->
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
    (hand, turn, card) =
      Alpha.evalI model $ do
        h <- Alpha.getHand which
        t <- Alpha.getTurn
        let c = atMay hand index :: Maybe Card
        return (h, t, c)


endTurn :: WhichPlayer -> Model -> Active.Replay -> Either Err (Maybe GameState, [Outcome])
endTurn which model replay
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | full          = Left "You can't end the turn when your hand is full"
  | otherwise     =
    case passes of
      OnePass ->
        case runWriter $ resolveAll model replay 0 of
          (Playing m newReplay, res) ->
            let
              roundEndProgram :: Beta.Program ()
              roundEndProgram = do
                Beta.raw Alpha.swapTurn
                Beta.raw Alpha.resetPasses
                Beta.draw PlayerA PlayerA 1
                Beta.draw PlayerB PlayerB 1
              (newModel, _, endRes) = Beta.execute m $ foldFree Beta.betaI roundEndProgram
              newPlayState = Playing newModel (Active.add newReplay endRes) :: PlayState
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
          newPlayState = Playing newModel replay :: PlayState
        in
          Right (
            Just . Started $ newPlayState,
            [
              Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing
            ]
          )
  where
    (turn, passes) =
      Alpha.evalI model $ do
        t <- Alpha.getTurn
        p <- Alpha.getPasses
        return (t, p)
    full :: Bool
    full = Alpha.evalI model $ Alpha.handFull which


resolveAll :: Model -> Active.Replay -> Int -> Writer [ResolveData] PlayState
resolveAll model replay resolutionCount = do
  let (modelA, _, resA) = Beta.execute model preProgram :: (Model, String, [ResolveData])
  tell resA
  let mStackCard = Alpha.evalI modelA (wheel_0 <$> Alpha.getStack)
  case mStackCard of
    Just stackCard -> do
      let (modelB, _, resB) = Beta.execute modelA (cardProgram stackCard) :: (Model, String, [ResolveData])
      tell resB
      let newReplay = replay `Active.add` resA `Active.add` resB
      case checkWin modelB newReplay of
        Playing m finalReplay ->
          resolveAll m finalReplay nextResolutionCount
        Ended w m finalReplay gen -> do
          let endRes = [resolveAnim $ GameEnd w]
          tell endRes
          return (Ended w m (finalReplay `Active.add` endRes) gen)
    Nothing -> do
      let newReplay = replay `Active.add` resA
      return (Playing modelA newReplay)
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
        when isFinite $ (card_eff stackcard_card) stackcard_owner
        holding <- Beta.getHold
        when (not holding) $ Beta.raw $ Alpha.modStack (\wheel -> wheel { wheel_0 = Nothing })


checkWin :: Model -> Active.Replay -> PlayState
checkWin m r
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing m r gen
  | lifePB <= 0 =
    Ended (Just PlayerA) m r gen
  | lifePA <= 0 =
    Ended (Just PlayerB) m r gen
  | otherwise =
    Playing m r
  where
    (gen, lifePA, lifePB) =
      Alpha.evalI m $ do
        g  <- Alpha.getGen
        la <- Alpha.getLife PlayerA
        lb <- Alpha.getLife PlayerB
        return (g, la, lb)


hoverCard :: HoverState -> WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
hoverCard (HoverHand i) which model =
  let
    hand = Alpha.evalI model $ Alpha.getHand which :: Hand
  in
    case atMay hand i of
      Just card ->
        Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (HoverHand i) damage ])
        where
          newModel = Alpha.modI model $ do
            Alpha.modHand which (deleteIndex i)
            Alpha.modStack (\s -> (Stack.windup s) { wheel_0 = Just $ StackCard which card })
          damage = Beta.damageNumbersI newModel $ card_eff card which
      Nothing ->
        ignore
hoverCard (HoverStack i) which model =
  let
    stack = Alpha.evalI model $ Alpha.getStack :: Stack
  in
    case atMay (toList stack) i of
      Just (Just (StackCard owner card)) ->
        Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (HoverStack i) damage ])
        where
          newModel = Alpha.modI model $ do
            Alpha.modStack $ times i (\s -> Stack.rotate (s { wheel_0 = Nothing }))
            Alpha.modRot ((-) i)
          damage = Beta.damageNumbersI newModel $ card_eff card owner
      _ ->
        ignore
hoverCard NoHover which _ =
  Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which NoHover (0, 0) ])


godMode :: Maybe User -> Text -> WhichPlayer -> Model -> Active.Replay -> Either Err (Maybe GameState, [Outcome])
godMode mUser str which model replay =
  if fromMaybe False $ isSuperuser <$> mUser then
    case GodMode.parse which str of
      Right betaProgram ->
        let
          program = foldFree Beta.betaI $ betaProgram :: Beta.AlphaLogAnimProgram ()
          (m, _, res) = Beta.execute model program :: (Model, String, [ResolveData])
          newPlayState = Playing m (Active.add replay res) :: PlayState
        in
          Right (
            Just . Started $ newPlayState
          , [Outcome.Encodable $ Outcome.Resolve res model newPlayState Nothing]
          )
      Left err ->
        Left err
  else
    Left $ "Not a superuser"


getUser :: WhichPlayer -> (Maybe User, Maybe User) -> Maybe User
getUser PlayerA (ua, _) = ua
getUser PlayerB (_, ub) = ub
