module GameCommand where

import Card (Card(..))
import CardAnim (CardAnim(..))
import Characters (CharModel(..), SelectedCharacters(..), selectChar, initCharModel)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GameState (GameState(..), PlayState(..), initModel)
import Model (Hand, Passes(..), Model, Turn)
import ModelDiff (ModelDiff)
import Outcome (Outcome)
import Player (WhichPlayer(..), other)
import Replay (Replay(..))
import Safe (atMay, headMay, tailSafe)
import StackCard(StackCard(..))
import Username (Username)
import Util (Err, Gen, deleteIndex, split)


import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import qualified ModelDiff
import qualified Outcome
import qualified Replay


data GameCommand =
    EndTurn
  | PlayCard Int
  | HoverCard (Maybe Int)
  | Rematch
  | Concede
  | SelectCharacter Text
  | Chat Username Text
  | PlayReplay
  deriving (Show)


update :: GameCommand -> WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ = chat username msg
update cmd which state =
  case state of
    Waiting _ _ ->
      Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter name ->
          select which name (selectModel, turn, gen)
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a selecting GameState")
    Started started ->
      case started of
        Playing model replay ->
          case cmd of
            EndTurn ->
              endTurn which model replay
            PlayCard index ->
              playCard index which model replay
            HoverCard index ->
              hoverCard index which model
            Concede ->
              concede which state
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        ps@(Ended winner _ replay gen) ->
          case cmd of
            Rematch ->
              rematch (winner, gen)
            HoverCard _ ->
              ignore
            PlayReplay ->
              playReplay replay ps
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


ignore :: Either Err (Maybe GameState, [Outcome])
ignore = Right (Nothing, [])


rematch :: (Maybe WhichPlayer, Gen) -> Either Err (Maybe GameState, [Outcome])
rematch (winner, gen) =
  Right (
    Just $ Selecting initCharModel (fromMaybe PlayerA winner) (fst $ split gen)
  , [ Outcome.Sync ]
  )


chat :: Username -> Text -> Either Err (Maybe GameState, [Outcome])
chat username msg =
  Right (
    Nothing
  , [ Outcome.Encodable $ Outcome.Chat username msg ]
  )


concede :: WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
concede which (Started (Playing model replay)) =
  let
    gen = Alpha.evalI model Alpha.getGen :: Gen
    anims = [(ModelDiff.base, Just (GameEnd (Just (other which))), Nothing)]
    newPlayState = Ended (Just (other which)) model replay gen :: PlayState
  in
    Right (
      Just . Started $ newPlayState
    , [ Outcome.Encodable $ Outcome.Resolve anims model newPlayState ]
    )
concede _ _ =
  Left "Cannot concede when not playing"


select :: WhichPlayer -> Text -> (CharModel, Turn, Gen) -> Either Err (Maybe GameState, [Outcome])
select which name (charModel, turn, gen) =
  Right (
    Just . startIfBothReady $ Selecting (selectChar charModel which name) turn gen
  , [ Outcome.Sync ]
  )
  where
    startIfBothReady :: GameState -> GameState
    startIfBothReady (Selecting (CharModel (ThreeSelected c1 c2 c3) (ThreeSelected ca cb cc) _) _ _) =
      let
        model = initModel turn (c1, c2, c3) (ca, cb, cc) gen :: Model
        replay = Replay.init model :: Replay
      in
        Started $ Playing model replay
    startIfBothReady s = s


playCard :: Int -> WhichPlayer -> Model -> Replay -> Either Err (Maybe GameState, [Outcome])
playCard index which m replay
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise     =
    case card of
      Nothing ->
        Left "You can't play a card you don't have in your hand"
      Just c ->
        let
          playCardProgram :: Beta.Program ()
          playCardProgram = do
            Beta.raw (do
              Alpha.swapTurn
              Alpha.resetPasses
              Alpha.modHand which (deleteIndex index))
            Beta.play which c
          (newModel, _, _, anims) = Beta.execute m $ foldFree Beta.betaI playCardProgram
          res :: [(ModelDiff, Maybe CardAnim, Maybe StackCard)]
          res = (\(x, y) -> (x, y, Nothing)) <$> anims
          newPlayState = Playing newModel (Replay.add replay res) :: PlayState
        in
          Right (
            Just . Started $ newPlayState,
            [
              Outcome.Encodable $ Outcome.Resolve res m newPlayState
            ]
          )
  where
    (hand, turn, card) =
      Alpha.evalI m $ do
        h <- Alpha.getHand which
        t <- Alpha.getTurn
        let c = atMay hand index :: Maybe Card
        return (h, t, c)


endTurn :: WhichPlayer -> Model -> Replay -> Either Err (Maybe GameState, [Outcome])
endTurn which model replay
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | full          = Left "You can't end the turn when your hand is full"
  | otherwise     =
    case passes of
      OnePass ->
        case runWriter $ resolveAll model replay of
          (Playing m newReplay, res) ->
            let
              endProgram :: Beta.Program ()
              endProgram = do
                  Beta.raw Alpha.swapTurn
                  Beta.raw Alpha.resetPasses
                  drawCards
              (newModel, _, _, endAnims) = Beta.execute m $ foldFree Beta.betaI endProgram
              endRes :: [(ModelDiff, Maybe CardAnim, Maybe StackCard)]
              endRes = (\(x, y) -> (x, y, Nothing)) <$> endAnims
              newPlayState :: PlayState
              newPlayState = Playing newModel (Replay.add newReplay endRes)
              newState = Started newPlayState :: GameState
            in
              Right (
                Just newState,
                [
                  Outcome.Encodable $ Outcome.Resolve (res ++ endRes) model newPlayState
                ]
              )
          (Ended w m newReplay g, res) ->
            let
              newPlayState = Ended w m newReplay g   :: PlayState
              newState     = Started newPlayState :: GameState
            in
              Right (
                Just newState,
                [
                  Outcome.Encodable $ Outcome.Resolve res model newPlayState
                ]
              )
      NoPass ->
        let
          newModel = Alpha.modI model Alpha.swapTurn :: Model
          newPlayState = Playing newModel replay :: PlayState
        in
          Right (
            Just . Started $ newPlayState,
            [
              Outcome.Encodable $ Outcome.Resolve [] model newPlayState
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
    drawCards :: Beta.Program ()
    drawCards = do
      Beta.draw PlayerA
      Beta.draw PlayerB


playReplay :: Replay -> PlayState -> Either Err (Maybe GameState, [Outcome])
playReplay replay state =
  Right (
    Nothing,
    [
      Outcome.Encodable $ Outcome.PlayReplay replay state
    ]
  )


resolveAll :: Model -> Replay -> Writer [(ModelDiff, Maybe CardAnim, Maybe StackCard)] PlayState
resolveAll model replay =
  case stackCard of
    Just c -> do
      let animsWithCard = (\(x, y) -> (x, y, Just c)) <$> anims
      tell animsWithCard
      case checkWin m (Replay.add replay animsWithCard) of
        Playing m' newReplay ->
          resolveAll m' newReplay
        Ended w m' newReplay gen -> do
          let endAnim = [(ModelDiff.base, Just (GameEnd w), Nothing)]
          tell endAnim
          return (Ended w m' (Replay.add newReplay endAnim) gen)
    Nothing ->
      return (Playing model replay)
  where
    stackCard :: Maybe StackCard
    stackCard = Alpha.evalI model (headMay <$> Alpha.getStack)
    card :: Maybe (Beta.Program ())
    card = (\(StackCard o c) -> (card_eff c) o) <$> stackCard
    program :: Beta.AlphaLogAnimProgram ()
    program = case card of
      Just p ->
        foldFree Beta.betaI $ do
          Beta.raw $ Alpha.modStack tailSafe
          p
      Nothing ->
        return ()
    (m, _, _, anims) = Beta.execute model program :: (Model, (), String, [(ModelDiff, Maybe CardAnim)])



checkWin :: Model -> Replay -> PlayState
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


hoverCard :: Maybe Int -> WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
hoverCard (Just i) which model
  | i >= (length (Alpha.evalI model $ Alpha.getHand which :: Hand) :: Int) =
    Left ("Hover index out of bounds (" <> (cs . show $ i ) <> ")" :: Err)
  | otherwise =
    Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (Just i) ])
hoverCard Nothing which _ =
  Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which Nothing ])
