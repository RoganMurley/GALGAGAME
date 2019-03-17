module DSL.Anim.Interpreters where

import CardAnim (CardAnim)
import DSL.Anim.DSL (DSL(..))

import qualified CardAnim as CardAnim


next :: DSL a -> a
next (Null n)          = n
next (Raw _ n)         = n
next (Slash _ _ n)     = n
next (Heal _ _ n)      = n
next (Draw _ n)        = n
next (Bite _ _ n)      = n
next (Hubris n)        = n
next (Reverse n)       = n
next (Reflect n)       = n
next (Play _ _ _ n)    = n
next (Transmute _ _ n) = n
next (Mill _ _ n)      = n
next (GameEnd _ n)     = n
next (Rotate n)        = n
next (Windup n)        = n
next (Fabricate _ n)   = n
next (Bounce _ n)      = n
next (Pass _ n)        = n


animate :: DSL a -> Maybe CardAnim
animate (Null _)            = Nothing
animate (Raw a _)           = Just a
animate (Slash w d _)       = Just $ CardAnim.Slash w d
animate (Heal w h _)        = Just $ CardAnim.Heal w h
animate (Draw w _)          = Just . CardAnim.Draw $ w
animate (Bite w d _)        = Just $ CardAnim.Bite w d
animate (Reflect _)         = Just CardAnim.Reflect
animate (Reverse _)         = Just CardAnim.Reverse
animate (Hubris _)          = Just CardAnim.Hubris
animate (Play w c i _)      = Just $ CardAnim.Play w c i
animate (Transmute ca cb _) = Just $ CardAnim.Transmute ca cb
animate (Mill w c _)        = Just $ CardAnim.Mill w c
animate (GameEnd w _)       = Just $ CardAnim.GameEnd w
animate (Rotate _)          = Just CardAnim.Rotate
animate (Windup _)          = Just CardAnim.Windup
animate (Fabricate c _)     = Just $ CardAnim.Fabricate c
animate (Bounce b _)        = Just $ CardAnim.Bounce b
animate (Pass w _)          = Just $ CardAnim.Pass w
