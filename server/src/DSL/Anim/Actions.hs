{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module DSL.Anim.Actions where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DSL.Anim.DSL (DSL(..))

makeFree ''DSL
