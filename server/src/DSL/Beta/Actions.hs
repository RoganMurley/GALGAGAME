{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Beta.Actions where

import CardAnim (Hurt(..))
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DSL.Beta.DSL (DSL(..), Program)
import Life (Life)
import Player (WhichPlayer(..), other)
import Safe (atMay)
import StackCard (StackCard(..))
import Transmutation (Transmutation(..))
import Util (shuffle, split)

import qualified DSL.Alpha as Alpha
import qualified Stack

makeFree ''DSL


lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal d w = do
  hurt d w Slash
  heal d (other w)


refreshGen :: Program ()
refreshGen = do
  gen <- getGen
  let (newGen, _) = split gen
  raw $ Alpha.setGen newGen


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
  let chain = fst <$> zip [1..] (Stack.chainToList stack) :: [Int]
  let shuffled = shuffle gen chain :: [Int]
  moveStack (\i _ -> if i > 0 then atMay shuffled (i - 1) else Nothing) 750
  return ()


reversal :: Program ()
reversal = do
  chainLen <- Stack.chainLength <$> getStack
  moveStack (\i _ -> if i > 0 then Just (chainLen - i + 1) else Nothing) 750
  return ()
