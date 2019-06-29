{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Beta.Actions where

import CardAnim (Hurt(..))
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DSL.Beta.DSL (DSL(..), Program)
import Life (Life)
import Player (WhichPlayer(..), other)

makeFree ''DSL


lifesteal :: Life -> WhichPlayer -> Program ()
lifesteal d w = do
  hurt d w Slash
  heal d (other w)
