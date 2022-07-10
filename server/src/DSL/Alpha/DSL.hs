{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSL.Alpha.DSL where

import Control.Monad.Freer (Eff)
import Life (Life)
import Model (Deck, Hand, Misc, Model, Passes, Turn)
import Player (WhichPlayer)
import Stack (Stack)
import Util (Gen)

data DSL n where
  GetGen :: DSL Gen
  GetDeck :: WhichPlayer -> DSL Deck
  GetHand :: WhichPlayer -> DSL Hand
  GetLife :: WhichPlayer -> DSL Life
  GetMaxLife :: WhichPlayer -> DSL Life
  GetPasses :: DSL Passes
  GetStack :: DSL Stack
  GetTurn :: DSL Turn
  GetRot :: DSL Int
  GetHold :: DSL Bool
  GetMisc :: DSL Misc
  GetModel :: DSL Model
  SetGen :: Gen -> DSL ()
  SetDeck :: WhichPlayer -> Deck -> DSL ()
  SetHand :: WhichPlayer -> Hand -> DSL ()
  SetLife :: WhichPlayer -> Life -> DSL ()
  SetMaxLife :: WhichPlayer -> Life -> DSL ()
  SetPasses :: Passes -> DSL ()
  SetStack :: Stack -> DSL ()
  SetTurn :: Turn -> DSL ()
  SetRot :: Int -> DSL ()
  SetHold :: Bool -> DSL ()
  SetMisc :: Misc -> DSL ()

type Program = Eff '[DSL]