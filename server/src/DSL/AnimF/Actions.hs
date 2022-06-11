{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module DSL.AnimF.Actions where

import Control.Monad.Freer.TH (makeEffect)
import DSL.AnimF.DSL (DSL (..))

makeEffect ''DSL
