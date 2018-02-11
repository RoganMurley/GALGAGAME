module DSL.Anim.Interpreters where

import DSL.Anim.DSL (DSL(..))

import qualified Model as Model


next :: DSL a -> a
next (Null n)       = n
next (Slash _ _ n)  = n
next (Heal _ n)     = n
next (Draw _ n)     = n
next (Obliterate n) = n
next (Reverse n)    = n
next (Play _ _ n)   = n


animate :: DSL a -> Maybe Model.CardAnim
animate (Null _)       = Nothing
animate (Slash w d _)  = Just $ Model.Slash w d
animate (Heal w _)     = Just . Model.Heal $ w
animate (Draw w _)     = Just . Model.Draw $ w
animate (Reverse _)    = Just Model.Reverse
animate (Obliterate _) = Just Model.Obliterate
animate (Play w c _)   = Just $ Model.Play w c
