module DSL.Beta.DSL where

import Control.Monad.Free (Free)

data DSL n

type Program = Free DSL
