{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module DSL.Anim.Actions where

import Control.Monad.Freer.TH (makeEffect)
import DSL.Anim.DSL (DSL (..))

makeEffect ''DSL
