{-# LANGUAGE GADTs #-}

module DSL.Anim.Interpreters where

import CardAnim (CardAnim)
import qualified CardAnim
import DSL.Anim.DSL (DSL (..))

animate :: DSL a -> Maybe CardAnim
animate Null = Nothing
animate (Raw a) = Just a
animate (Hurt w d h) = Just $ CardAnim.Hurt w d h
animate (Heal w h) = Just $ CardAnim.Heal w h
animate (Draw w t) = Just $ CardAnim.Draw w t
animate (Play w c i) = Just $ CardAnim.Play w c i
animate (Transmute t) = Just $ CardAnim.Transmute t
animate (Mill w c t) = Just $ CardAnim.Mill w c t
animate (GameEnd w) = Just $ CardAnim.GameEnd w
animate Rotate = Just CardAnim.Rotate
animate Windup = Just CardAnim.Windup
animate (Bounce b t) = Just $ CardAnim.Bounce b t
animate (BounceDeck b) = Just $ CardAnim.BounceDeck b
animate (DiscardStack d) = Just $ CardAnim.DiscardStack d
animate (DiscardHand w d) = Just $ CardAnim.DiscardHand w d
animate (MoveStack m t) = Just $ CardAnim.MoveStack m t
animate (Reveal w r) = Just $ CardAnim.Reveal w r
animate (Pass w) = Just $ CardAnim.Pass w
animate (Announce a t) = Just $ CardAnim.Announce a t
animate GetGen = Just CardAnim.GetGen
animate UnknownDamage = Just CardAnim.UnknownDamage

next :: DSL a -> a
next Null = ()
next Raw {} = ()
next Hurt {} = ()
next Heal {} = ()
next (Draw _ _) = ()
next Play {} = ()
next Transmute {} = ()
next Mill {} = ()
next GameEnd {} = ()
next Rotate = ()
next Windup = ()
next Bounce {} = ()
next BounceDeck {} = ()
next DiscardStack {} = ()
next DiscardHand {} = ()
next MoveStack {} = ()
next Reveal {} = ()
next Pass {} = ()
next Announce {} = ()
next GetGen = ()
next UnknownDamage = ()
