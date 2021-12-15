module StatusEff where

import CardAnim (Hurt(..))
import Control.Monad.Free (hoistFree)
import Card (Card(..), Status(..))
import DSL.Beta (DSL(..))

import qualified DSL.Beta as Beta


applyStatuses :: Card -> Card
applyStatuses card =
  case card_statuses card of
    (status : statuses) ->
      applyStatuses $ card {
        card_eff = \w -> statusEff status (card_eff card w),
        card_statuses = statuses
      }
    [] ->
      card


statusEff :: Status -> (Beta.Program a -> Beta.Program a)
statusEff StatusEcho     = \eff -> eff >> eff
statusEff StatusBlighted = hoistFree blightedRewrite
statusEff StatusNegate   = hoistFree negateRewrite


blightedRewrite :: Beta.DSL a -> Beta.DSL a
blightedRewrite (Heal l w n) = Hurt l w Curse n
blightedRewrite dsl = dsl


negateRewrite :: Beta.DSL a -> Beta.DSL a
negateRewrite (Raw _ n)             = Null n
negateRewrite (Hurt _ _ _ n)        = Null n
negateRewrite (Heal _ _ n)          = Null n
negateRewrite (Draw _ _ _ n)        = Null n
negateRewrite (AddToHand _ _ n)     = Null n
negateRewrite (Play _ _ _ n)        = Null n
negateRewrite (Transmute _ n)       = Null n
negateRewrite (TransmuteActive _ n) = Null n
negateRewrite (Rotate n)            = Null n
negateRewrite (Windup n)            = Null n
negateRewrite (Bounce _ n)          = Null n
negateRewrite (DiscardStack _ n)    = Null n
negateRewrite (DiscardHand _ _ n)   = Null n
negateRewrite (MoveStack _ _ n)     = Null n
negateRewrite (Mill _ _ n)          = Null n
negateRewrite (Reveal _ _ n)        = Null n
negateRewrite (GetDeck w f)         = GetDeck w f
negateRewrite (GetHand w f)         = GetHand w f
negateRewrite (GetLife w f)         = GetLife w f
negateRewrite (GetGen f)            = GetGen f
negateRewrite (GetStack f)          = GetStack f
negateRewrite (GetRot f)            = GetRot f
negateRewrite (GetHold f)           = GetHold f
negateRewrite (GetModel f)          = GetModel f
negateRewrite (RawAnim _ n)         = Null n
negateRewrite (Null n)              = Null n
