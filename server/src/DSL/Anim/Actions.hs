{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module DSL.Anim.Actions where

import Control.Monad.Freer.TH (makeEffect)
import DSL.Anim.DSL (DSL (..))

makeEffect ''DSL
