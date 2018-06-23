module DSL.Anim.Interpreters where

import CardAnim (CardAnim)
import DSL.Anim.DSL (DSL(..))

import qualified CardAnim as CardAnim


next :: DSL a -> a
next (Null n)          = n
next (Raw _ n)         = n
next (Slash _ _ n)     = n
next (Heal _ n)        = n
next (Draw _ n)        = n
next (Bite _ _ n)      = n
next (Obliterate n)    = n
next (Reverse n)       = n
next (Play _ _ n)      = n
next (Transmute _ _ n) = n
next (Overdraw _ _ n)  = n
next (GameEnd _ n)     = n


animate :: DSL a -> Maybe CardAnim
animate (Null _)            = Nothing
animate (Raw a _)           = Just a
animate (Slash w d _)       = Just $ CardAnim.Slash w d
animate (Heal w _)          = Just . CardAnim.Heal $ w
animate (Draw w _)          = Just . CardAnim.Draw $ w
animate (Bite w d _)        = Just $ CardAnim.Bite w d
animate (Reverse _)         = Just CardAnim.Reverse
animate (Obliterate _)      = Just CardAnim.Obliterate
animate (Play w c _)        = Just $ CardAnim.Play w c
animate (Transmute ca cb _) = Just $ CardAnim.Transmute ca cb
animate (Overdraw w c _)    = Just $ CardAnim.Overdraw w c
animate (GameEnd w _)       = Just $ CardAnim.GameEnd w
