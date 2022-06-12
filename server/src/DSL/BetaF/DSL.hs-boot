module DSL.BetaF.DSL where

import Control.Monad.Freer (Eff)

data DSL n

type Program = Eff '[DSL]
