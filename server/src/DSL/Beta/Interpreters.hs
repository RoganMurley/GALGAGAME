{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DSL.Beta.Interpreters where

import Control.Monad.Free (Free(..), foldFree, liftF)
import Data.Functor.Sum (Sum(..))
import DSL.Beta.DSL
import DSL.Util (toLeft, toRight)
import Player (WhichPlayer(..))
import Model (CardAnim, Model, maxHandLength)

import qualified DSL.Alpha as Alpha
import qualified DSL.Anim as Anim
import qualified DSL.Log as Log


alphaI :: Program a -> Alpha.Program a
alphaI (Free (Raw p n))         = p                      >>  alphaI n
alphaI (Free (Slash d w n))     = Alpha.hurt d w         >>  alphaI n
alphaI (Free (Heal h w n))      = Alpha.heal h w         >>  alphaI n
alphaI (Free (Draw w n))        = Alpha.draw w           >>  alphaI n
alphaI (Free (AddToHand w c n)) = Alpha.addToHand w c    >>  alphaI n
alphaI (Free (Obliterate n))    = Alpha.setStack []      >>  alphaI n
alphaI (Free (Reverse n))       = Alpha.modStack reverse >>  alphaI n
alphaI (Free (Play w c n))      = Alpha.play w c         >>  alphaI n
alphaI (Free (GetGen f))        = Alpha.getGen           >>= alphaI . f
alphaI (Free (GetLife w f))     = Alpha.getLife w        >>= alphaI . f
alphaI (Free (GetHand w f))     = Alpha.getHand w        >>= alphaI . f
alphaI (Free (GetDeck w f))     = Alpha.getDeck w        >>= alphaI . f
alphaI (Free (GetStack f))      = Alpha.getStack         >>= alphaI . f
alphaI (Free (Null n))          = alphaI n
alphaI (Pure x)                 = Pure x



animI :: DSL a -> ((Alpha.Program a) -> AlphaAnimProgram a)
animI (Null _)          = \a -> (toLeft a) <* (toRight . liftF $ Anim.Null ())
animI (Slash d w _)     = \a -> (toLeft a) <* (toRight . liftF $ Anim.Slash w d ())
animI (Heal _ w _)      = \a -> (toLeft a) <* (toRight . liftF $ Anim.Heal w ())
animI (AddToHand w _ _) = drawAnim w
animI (Draw w _)        = drawAnim w
animI (Obliterate _)    = \a -> (toLeft a) <* (toRight . liftF $ Anim.Obliterate ())
animI (Reverse _)       = \a -> (toLeft a) <* (toRight . liftF $ Anim.Reverse ())
animI (Play w c _)      = \a -> (toLeft a) <* (toRight . liftF $ Anim.Play w c ())
animI _                 = toLeft


drawAnim :: WhichPlayer -> ((Alpha.Program a) -> AlphaAnimProgram a)
drawAnim w alpha =
  do
    handLength <- length <$> toLeft (Alpha.getHand w)
    final <- toLeft alpha
    if (handLength < maxHandLength)
      then toRight . liftF $ Anim.Draw w ()
      else toRight . liftF $ Anim.Null ()
    return final


type AlphaAnimProgram = Free (Sum Alpha.DSL Anim.DSL)
type AlphaLogAnimProgram = Free (Sum (Sum Alpha.DSL Log.DSL) Anim.DSL)


liftAlphaAnim :: ∀ a . Sum Alpha.DSL Anim.DSL a -> AlphaLogAnimProgram a
liftAlphaAnim (InL alpha) = toLeft $ Alpha.decorateLog alpha
liftAlphaAnim (InR anim)  = toRight $ liftF anim


betaI :: ∀ a . DSL a -> AlphaLogAnimProgram a
betaI x = (foldFree liftAlphaAnim) . (animI x) . alphaI $ liftF x


execute :: Model -> AlphaLogAnimProgram a -> (Model, a, String, [(Model, Maybe CardAnim)])
execute = execute' "" []
  where
    execute' :: String -> [(Model, Maybe CardAnim)] -> Model -> AlphaLogAnimProgram a -> (Model, a, String, [(Model, Maybe CardAnim)])
    execute' s a m (Pure x) =
      (m, x, s, a)
    execute' s a m (Free (InR anim)) =
      execute' s (a ++ [(m, Anim.animate anim)]) m (Anim.next anim)
    execute' s a m (Free (InL (InL p)))  =
      (uncurry (execute' s a)) (Alpha.alphaEffI m p)
    execute' s a m (Free (InL (InR (Log.Log l n)))) =
      execute' (s ++ l ++ "\n") a m n
