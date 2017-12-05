module DSL.Beta where

import Control.Monad.Free (Free(..), liftF)
import qualified DSL.Alpha as Alpha
import Text.Printf (printf)

import qualified Model
import Player (WhichPlayer(..))


data DSL next =
    Hurt Int WhichPlayer next
  | Heal Int WhichPlayer next
  | Lifesteal Int WhichPlayer next
  | Draw WhichPlayer next
  deriving (Functor, Show)


type Program = Free DSL ()


hurt :: Int -> WhichPlayer -> Program
hurt x w = liftF $ Hurt x w ()


heal :: Int -> WhichPlayer -> Program
heal x w = liftF $ Heal x w ()


draw :: WhichPlayer -> Program
draw w = liftF $ Draw w ()


lifesteal :: Int -> WhichPlayer -> Program
lifesteal x w = liftF $ Lifesteal x w ()


program :: Program
program = do
  hurt 10 PlayerA
  heal 10 PlayerB
  lifesteal 10 PlayerA


interpEff :: Program -> (Model.Model -> Model.Model)
interpEff = Alpha.interpEff . interpAlpha


interpLog :: Program -> String
interpLog (Free (Hurt x _ next))      = printf "Hurt for %d\n" x ++ interpLog next
interpLog (Free (Heal x _ next))      = printf "Heal for %d\n" x ++ interpLog next
interpLog (Free (Lifesteal x _ next)) = printf "Lifesteal for %d\n" x ++ interpLog next
interpLog (Free (Draw _ next))        = "Draw a card\n" ++ interpLog next
interpLog (Pure ())                   = ""


interpAlpha :: Program -> Alpha.Program
interpAlpha (Free (Hurt x w next))      = do { Alpha.hurt x w; interpAlpha next }
interpAlpha (Free (Heal x w next))      = do { Alpha.heal x w; interpAlpha next }
interpAlpha (Free (Lifesteal x w next)) = do { Alpha.hurt x w; Alpha.heal x w; interpAlpha next }
interpAlpha (Free (Draw w next))        = do { Alpha.draw w; interpAlpha next }
interpAlpha (Pure ())                   = Pure ()
