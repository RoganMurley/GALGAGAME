{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Beta.Actions where

import CardAnim (Hurt(..))
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Map (Map)
import DSL.Beta.DSL (DSL(..), Program)
import HandCard (HandCard, isRevealed)
import Life (Life)
import Player (WhichPlayer(..), other)
import StackCard (StackCard(..))
import Transmutation (Transmutation(..))
import Util (randomChoice, shuffle, split)

import qualified Data.Map as Map
import qualified DSL.Alpha as Alpha
import qualified Stack

makeFree ''DSL


lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal d w = do
  hurt d w Slash
  heal d (other w)


refreshGen :: Program ()
refreshGen =
  raw $ do
    gen <- Alpha.getGen
    let (newGen, _) = split gen
    Alpha.setGen newGen


transmuteHead :: (StackCard -> StackCard) -> Program ()
transmuteHead f = transmute transmuter
  where
    transmuter :: Int -> StackCard -> Maybe Transmutation
    transmuter 1 sc = Just $ Transmutation sc (f sc)
    transmuter _ _  = Nothing


confound :: Program ()
confound = do
  gen <- getGen
  stack <- getStack
  let diasporaIs = drop 1 $ fst <$> Stack.diasporaFromStack stack :: [Int]
  let shuffledIs = shuffle gen diasporaIs :: [Int]
  let diasporaMap = Map.fromList $ zip diasporaIs shuffledIs :: Map Int Int
  moveStack (\i _ -> Map.lookup i diasporaMap) 750


reversal :: Program ()
reversal = do
  diaspora <- Stack.diasporaFromStack <$> getStack
  let diasporaIs = drop 1 $ fst <$> diaspora :: [Int]
  let reversedIs = reverse diasporaIs :: [Int]
  let diasporaMap = Map.fromList $ zip diasporaIs reversedIs :: Map Int Int
  moveStack (\i _ -> Map.lookup i diasporaMap) 750


revealRandomCard :: WhichPlayer -> Program ()
revealRandomCard w = do
  hand <- getHand (other w)
  let indexed = zip [0..] hand :: [(Int, HandCard)]
  let hidden = filter (\(_, c) -> not (isRevealed c)) indexed  :: [(Int, HandCard)]
  case length hidden > 0 of
    True -> do
      g <- getGen
      let targetIndex = fst $ randomChoice g hidden
      reveal (other w) (\i _ -> i == targetIndex)
    False ->
      return ()
