module GameCommand where

import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (atMay, headMay, tailSafe)

import Characters (CharModel(..), SelectedCharacters(..), selectChar, initCharModel)
import GameState (GameState(..), PlayState(..), initModel)
import Model
import Player (WhichPlayer(..), other)
import Username (Username)
import Util (Err, Gen, deleteIndex, split)

import qualified Outcome as Outcome
import Outcome (Outcome)


data GameCommand =
    EndTurn
  | PlayCard Int
  | HoverCard (Maybe Int)
  | Rematch
  | Concede
  | SelectCharacter Text
  | Chat Username Text
  deriving (Show)


update :: GameCommand -> WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _     _     = chat    username msg
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
        Playing model ->
          case cmd of
            EndTurn ->
              endTurn which model
            PlayCard index ->
              playCard index which model
            HoverCard index ->
              hoverCard index which model
            Concede ->
              concede which state
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner _ gen ->
          case cmd of
            Rematch ->
              rematch (winner, gen)
            HoverCard _ ->
              ignore
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
concede which (Started (Playing model)) =
  Right (
    Just . Started $ Ended (Just (other which)) model (evalI model getGen)
  , [ Outcome.Sync ]
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
      Started . Playing $ initModel turn (c1, c2, c3) (ca, cb, cc) gen
    startIfBothReady s = s


playCard :: Int -> WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
playCard index which m
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise     =
    case card of
      Nothing ->
        Left "You can't play a card you don't have in your hand"
      Just c ->
        Right (
            Just
              . Started
                . Playing
                  $ modI m
                    $ do
                      swapTurn
                      resetPasses
                      modStack ((:) c)
                      modHand which (deleteIndex index)
          , [
            Outcome.Sync,
            Outcome.PlayCard which
          ]
        )
  where
    (hand, turn, card) =
      evalI m $ do
        h <- getHand which
        t <- getTurn
        let c = (StackCard which) <$> (atMay hand index) :: Maybe StackCard
        return (h, t, c)



endTurn :: WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
endTurn which model
  | turn /= which = Left "You can't end the turn when it's not your turn"
  | full          = Left "You can't end the turn when your hand is full"
  | otherwise     =
    case passes of
      OnePass ->
        case runWriter . resolveAll $ model of
          (Playing m, res) ->
            let
              endProgram :: BetaProgram ()
              endProgram = do
                  betaRaw swapTurn
                  betaRaw resetPasses
                  drawCards
              (newModel, _, _, endAnims) = execute m $ foldFree betaI endProgram
              newPlayState :: PlayState
              newPlayState = Playing newModel
              newState = Started newPlayState :: GameState
              endRes :: [(Model, Maybe CardAnim, Maybe StackCard)]
              endRes = (\(x, y) -> (x, y, Nothing)) <$> endAnims
            in
              Right (
                Just newState,
                [
                  Outcome.Encodable $ Outcome.Resolve (res ++ endRes) newPlayState,
                  Outcome.EndTurn which
                ]
              )
          (Ended w m g, res) ->
            let
              newPlayState = Ended w m g          :: PlayState
              newState     = Started newPlayState :: GameState
            in
              Right (
                Just newState,
                [
                  Outcome.Encodable $ Outcome.Resolve res newPlayState,
                  Outcome.EndTurn which
                ]
              )
      NoPass ->
        Right (
          Just $ Started $ Playing $ modI model swapTurn,
          [
            Outcome.Sync
          ]
        )
  where
    (turn, passes) =
      evalI model $ do
        t <- getTurn
        p <- getPasses
        return (t, p)
    full :: Bool
    full = evalI model $ handFull which
    drawCards :: BetaProgram ()
    drawCards = do
      betaDraw PlayerA
      betaDraw PlayerB


resolveAll :: Model -> Writer [(Model, Maybe CardAnim, Maybe StackCard)] PlayState
resolveAll model =
  case stackCard of
    Just c -> do
      tell ((\(x, y) -> (x, y, Just c)) <$> anims)
      case checkWin m of
        Playing m' ->
          resolveAll m'
        Ended w m' gen ->
          return (Ended w m' gen)
    Nothing ->
      return (Playing model)
  where
    stackCard :: Maybe StackCard
    stackCard = evalI model (headMay <$> getStack)
    card :: Maybe (BetaProgram ())
    card = (\(StackCard o c) -> (card_eff c) o) <$> stackCard
    program :: AlphaLogAnimProgram ()
    program = case card of
      Just betaProgram ->
        foldFree betaI $ do
          betaRaw $ modStack tailSafe
          betaProgram
      Nothing ->
        return ()
    (m, _, _, anims) = execute model program :: (Model, (), String, [(Model, Maybe CardAnim)])



checkWin :: Model -> PlayState
checkWin m
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing m gen
  | lifePB <= 0 =
    Ended (Just PlayerA) m gen
  | lifePA <= 0 =
    Ended (Just PlayerB) m gen
  | otherwise =
    Playing m
  where
    (gen, lifePA, lifePB) =
      evalI m $ do
        g  <- getGen
        la <- getLife PlayerA
        lb <- getLife PlayerB
        return (g, la, lb)


hoverCard :: Maybe Int -> WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
hoverCard (Just i) which model
  | i >= (length (evalI model $ getHand which :: Hand) :: Int) =
    Left ("Hover index out of bounds (" <> (cs . show $ i ) <> ")" :: Err)
  | otherwise =
    Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which (Just i) ])
hoverCard Nothing which _ =
  Right (Nothing, [ Outcome.Encodable $ Outcome.Hover which Nothing ])
