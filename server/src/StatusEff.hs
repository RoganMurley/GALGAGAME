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
statusEff StatusBlighted = hoistFree  blightedRewrite


blightedRewrite :: Beta.DSL a -> Beta.DSL a
blightedRewrite (Heal l w n) = Hurt l w Slash n
blightedRewrite dsl = dsl
