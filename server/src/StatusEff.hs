{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module StatusEff where

import Card (Card (..), Status (..))
import CardAnim (Hurt (..))
import Control.Monad.Freer (reinterpret, send)
import DSL.Beta (DSL (..), getLife)
import DSL.Beta qualified as Beta

applyStatuses :: Card -> Card
applyStatuses card =
  case card_statuses card of
    (status : statuses) ->
      applyStatuses $
        card
          { card_eff = \w -> statusEff status (card_eff card w),
            card_statuses = statuses
          }
    [] ->
      card

statusEff :: Status -> (Beta.Program () -> Beta.Program ())
statusEff StatusEcho = \eff -> eff >> eff
statusEff StatusBlighted = reinterpret blightedRewrite
statusEff StatusFragile = id
statusEff (StatusBonusDamage d) = reinterpret (bonusDamageRewrite d)
statusEff StatusNonLethal = reinterpret nonLethalRewrite

blightedRewrite :: Beta.DSL a -> Beta.Program a
blightedRewrite (Heal l w) = send $ Hurt l w Curse
blightedRewrite dsl = send dsl

bonusDamageRewrite :: Int -> Beta.DSL a -> Beta.Program a
bonusDamageRewrite bonus (Hurt d w h) = send $ Hurt (d + bonus) w h
bonusDamageRewrite _ dsl = send dsl

nonLethalRewrite :: Beta.DSL a -> Beta.Program a
nonLethalRewrite (Hurt d w h) = do
  life <- getLife w
  send $ Hurt (min d (life - 1)) w h
nonLethalRewrite dsl = send dsl
