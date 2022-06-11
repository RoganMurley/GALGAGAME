{-# LANGUAGE GADTs #-}

module DSL.AnimF.Interpreters where

import CardAnim (CardAnim)
import qualified CardAnim
import DSL.AnimF.DSL (DSL (..))

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
animate (DiscardStack d) = Just $ CardAnim.DiscardStack d
animate (DiscardHand w d) = Just $ CardAnim.DiscardHand w d
animate (MoveStack m t) = Just $ CardAnim.MoveStack m t
animate (Reveal w r) = Just $ CardAnim.Reveal w r
animate (Pass w) = Just $ CardAnim.Pass w
animate GetGen = Just CardAnim.GetGen
