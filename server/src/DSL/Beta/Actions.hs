{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Beta.Actions where

import CardAnim (Hurt(..))
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DSL.Beta.DSL (DSL(..), Program)
import Life (Life)
import Player (WhichPlayer(..), other)
import StackCard (StackCard(..))
import Transmutation (Transmutation(..))
import Util (split)

import qualified DSL.Alpha as Alpha

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
