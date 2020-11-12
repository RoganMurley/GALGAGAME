module DSL.Anim.Interpreters where

import CardAnim (CardAnim)
import DSL.Anim.DSL (DSL(..))

import qualified CardAnim as CardAnim


next :: DSL a -> a
next (Null n)            = n
next (Raw _ n)           = n
next (Hurt _ _ _ n)      = n
next (Heal _ _ n)        = n
next (Draw _ n)          = n
next (Reverse n)         = n
next (Confound n)        = n
next (Play _ _ _ n)      = n
next (Transmute _ n)     = n
next (Mill _ _ n)        = n
next (GameEnd _ n)       = n
next (Rotate n)          = n
next (Windup n)          = n
next (Fabricate _ n)     = n
next (Bounce _ n)        = n
next (DiscardStack _ n)  = n
next (DiscardHand _ _ n) = n
next (MoveStack _ n)     = n
next (Pass _ n)          = n


animate :: DSL a -> Maybe CardAnim
animate (Null _)            = Nothing
animate (Raw a _)           = Just a
animate (Hurt w d h _)      = Just $ CardAnim.Hurt w d h
animate (Heal w h _)        = Just $ CardAnim.Heal w h
animate (Draw w _)          = Just $ CardAnim.Draw $ w
animate (Play w c i _)      = Just $ CardAnim.Play w c i
animate (Transmute t _)     = Just $ CardAnim.Transmute t
animate (Mill w c _)        = Just $ CardAnim.Mill w c
animate (GameEnd w _)       = Just $ CardAnim.GameEnd w
animate (Rotate _)          = Just $ CardAnim.Rotate
animate (Windup _)          = Just $ CardAnim.Windup
animate (Fabricate c _)     = Just $ CardAnim.Fabricate c
animate (Bounce b _)        = Just $ CardAnim.Bounce b
animate (DiscardStack d _)  = Just $ CardAnim.DiscardStack d
animate (DiscardHand w d _) = Just $ CardAnim.DiscardHand w d
animate (MoveStack m _)     = Just $ CardAnim.MoveStack m
animate (Pass w _)          = Just $ CardAnim.Pass w
