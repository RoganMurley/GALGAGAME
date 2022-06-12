{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module DSL.Beta.DSL where

import Control.Monad.Freer (Eff)

type role DSL nominal

data DSL n

type Program = Eff '[DSL]
