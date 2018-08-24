{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DSL.Beta.Interpreters where

import Card (Card)
import CardAnim (CardAnim)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Data.Functor.Sum (Sum(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import DSL.Beta.DSL
import DSL.Util (toLeft, toRight)
import Player (WhichPlayer(..))

import Model (Model, gameover, maxHandLength)
import ModelDiff (ModelDiff)
import Safe (headMay, tailSafe)
import StackCard (StackCard(..))

import qualified DSL.Alpha as Alpha
import qualified DSL.Anim as Anim
import qualified DSL.Log as Log
import qualified ModelDiff

import {-# SOURCE #-} Cards (theEnd)


alphaI :: Program a -> Alpha.Program a
alphaI (Free (Raw p n))          = p                       >>  alphaI n
alphaI (Free (Slash d w n))      = Alpha.hurt d w          >>  alphaI n
alphaI (Free (Heal h w n))       = Alpha.heal h w          >>  alphaI n
alphaI (Free (Draw w n))         = Alpha.draw w            >>  alphaI n
alphaI (Free (Bite d w n))       = Alpha.hurt d w          >>  alphaI n
alphaI (Free (AddToHand w c n))  = Alpha.addToHand w c     >>  alphaI n
alphaI (Free (Obliterate n))     = Alpha.setStack []       >>  alphaI n
alphaI (Free (Reverse n))        = Alpha.modStack reverse  >>  alphaI n
alphaI (Free (Play w c n))       = Alpha.play w c          >>  alphaI n
alphaI (Free (Transmute c n))    = Alpha.transmute c       >>  alphaI n
alphaI (Free (Rotate n))         = Alpha.modStack tailSafe >>  alphaI n
alphaI (Free (SetHeadOwner w n)) = Alpha.setHeadOwner w    >>  alphaI n
alphaI (Free (GetGen f))         = Alpha.getGen            >>= alphaI . f
alphaI (Free (GetLife w f))      = Alpha.getLife w         >>= alphaI . f
alphaI (Free (GetHand w f))      = Alpha.getHand w         >>= alphaI . f
alphaI (Free (GetDeck w f))      = Alpha.getDeck w         >>= alphaI . f
alphaI (Free (GetStack f))       = Alpha.getStack          >>= alphaI . f
alphaI (Free (RawAnim _ n))      = alphaI n
alphaI (Free (Null n))           = alphaI n
alphaI (Pure x)                  = Pure x



animI :: DSL a -> ((Alpha.Program a) -> AlphaAnimProgram a)
animI (Null _)           = \a -> (toLeft a) <* (toRight . liftF $ Anim.Null ())
animI (Slash d w _)      = \a -> (toLeft a) <* (toRight . liftF $ Anim.Slash w d ())
animI (Heal _ w _)       = \a -> (toLeft a) <* (toRight . liftF $ Anim.Heal w ())
animI (AddToHand w _ _)  = drawAnim w
animI (Draw w _)         = drawAnim w
animI (Bite d w _)       = \a -> (toLeft a) <* (toRight . liftF $ Anim.Bite w d ())
animI (Obliterate _)     = \a -> (toLeft a) <* (toRight . liftF $ Anim.Obliterate ())
animI (Reverse _)        = \a -> (toLeft a) <* (toRight . liftF $ Anim.Reverse ())
animI (Play w c _)       = \a -> (toLeft a) <* (toRight . liftF $ Anim.Play w c ())
animI (Rotate _)         = \a -> (toLeft a) <* (toRight . liftF $ Anim.Rotate ())
animI (Transmute c _)    = transmuteAnim c
animI (SetHeadOwner w _) = setHeadOwnerAnim w
animI (RawAnim r _)      = \a -> (toLeft a) <* (toRight . liftF $ Anim.Raw r ())
animI _                  = toLeft


drawAnim :: WhichPlayer -> ((Alpha.Program a) -> AlphaAnimProgram a)
drawAnim w alpha =
  do
    nextCard <- headMay <$> toLeft (Alpha.getDeck w)
    handLength <- length <$> toLeft (Alpha.getHand w)
    final <- toLeft alpha
    if (handLength < maxHandLength)
      then toRight . liftF $ Anim.Draw w ()
      else toRight . liftF $ Anim.Overdraw w (fromMaybe theEnd nextCard) ()
    return final


transmuteAnim :: Card -> ((Alpha.Program a) -> AlphaAnimProgram a)
transmuteAnim cb alpha =
  do
    stackHead <- headMay <$> toLeft Alpha.getStack
    final <- toLeft alpha
    case stackHead of
      (Just ca) ->
        let o = stackcard_owner ca in
        toRight . liftF $ Anim.Transmute ca (StackCard o cb) ()
      Nothing ->
        toRight . liftF $ Anim.Null ()
    return final


setHeadOwnerAnim :: WhichPlayer -> ((Alpha.Program a) -> AlphaAnimProgram a)
setHeadOwnerAnim w alpha =
  do
    stackHead <- headMay <$> toLeft Alpha.getStack
    final <- toLeft alpha
    case stackHead of
      (Just (StackCard o c)) ->
        toRight . liftF $ Anim.Transmute (StackCard o c) (StackCard w c) ()
      Nothing ->
        toRight . liftF $ Anim.Null ()
    return final


type AlphaAnimProgram = Free (Sum Alpha.DSL Anim.DSL)
type AlphaLogAnimProgram = Free (Sum (Sum Alpha.DSL Log.DSL) Anim.DSL)


liftAlphaAnim :: ∀ a . Sum Alpha.DSL Anim.DSL a -> AlphaLogAnimProgram a
liftAlphaAnim (InL alpha) = toLeft $ Alpha.decorateLog alpha
liftAlphaAnim (InR anim)  = toRight $ liftF anim


betaI :: ∀ a . DSL a -> AlphaLogAnimProgram a
betaI x = (foldFree liftAlphaAnim) . (animI x) . alphaI $ liftF x


execute :: Model -> AlphaLogAnimProgram () -> (Model, String, [(ModelDiff, Maybe CardAnim)])
execute = execute' "" [] mempty
  where
    execute' :: String -> [(ModelDiff, Maybe CardAnim)] -> ModelDiff -> Model -> AlphaLogAnimProgram () -> (Model, String, [(ModelDiff, Maybe CardAnim)])
    execute' s a _ m (Pure _) =
      (m, s, a)
    execute' s a d m (Free (InR anim)) =
      let
        next = if gameover m then Pure () else Anim.next anim
      in
          execute' s (a ++ [(d, Anim.animate anim)]) mempty m next
    execute' s a d m (Free (InL (InL p))) =
      let
         (newDiff, n) = Alpha.alphaEffI m p
         newModel = ModelDiff.update m newDiff
      in
        execute' s a (d <> newDiff) newModel n
    execute' s a d m (Free (InL (InR (Log.Log l n)))) =
      execute' (s ++ l ++ "\n") a d m n
