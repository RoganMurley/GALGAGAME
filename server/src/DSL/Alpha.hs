module DSL.Alpha where

import Control.Monad.Free (Free(..), liftF)
import Text.Printf (printf)

import qualified Model
import Player (WhichPlayer(..))


data DSL next =
    Hurt Int WhichPlayer next
  | Heal Int WhichPlayer next
  | Draw WhichPlayer next
  deriving (Functor, Show)


type Program = Free DSL ()


hurt :: Int -> WhichPlayer -> Program
hurt x w = liftF $ Hurt x w ()


heal :: Int -> WhichPlayer -> Program
heal x w = liftF $ Heal x w ()


draw :: WhichPlayer -> Program
draw w = liftF $ Draw w ()


program :: Program
program = do
  hurt 10 PlayerA
  heal 10 PlayerB


interpEff :: Program -> (Model.Model -> Model.Model)
interpEff (Free (Hurt x w next)) = (Model.hurt x w)   . (interpEff next)
interpEff (Free (Heal x w next)) = (Model.heal x w)   . (interpEff next)
interpEff (Free (Draw w next))   = (Model.drawCard w) . (interpEff next)
interpEff (Pure ())              = id


interpLog :: Program -> String
interpLog (Free (Hurt x _ next)) = printf "Hurt for %d\n" x ++ interpLog next
interpLog (Free (Heal x _ next)) = printf "Heal for %d\n" x ++ interpLog next
interpLog (Free (Draw _ next))   = "Draw a card\n" ++ interpLog next
interpLog (Pure ())              = ""
