{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module DSL.Log where

import Control.Monad.Free (Free (..), MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)

data DSL a
  = Log String a
  deriving (Functor)

type Program a = Free DSL a

makeFree ''DSL
