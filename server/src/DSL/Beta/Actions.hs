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
    transmuter 0 sc = Just $ Transmutation sc (f sc)
    transmuter _ _  = Nothing

confound :: Program ()
confound = do
  gen <- getGen
  stack <- getStack
  let chain = fst <$> zip [0..] (Stack.chainToList stack) :: [Int]
  let shuffled = shuffle gen chain :: [Int]
  moveStack (\i _ -> atMay shuffled i)
  return ()


reversal :: Program ()
reversal = do
  stackLen <- length <$> getStack
  moveStack (\i _ -> Just $ stackLen - 1 - i)
  return ()
