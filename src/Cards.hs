module Cards where

import Data.Monoid ((<>))

import Model
import Util (shuffle, times)


cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 8" "plain-dagger.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 8 (otherPlayer p) m


cardHubris :: Card
cardHubris = Card "Hubris" "Negate all cards to the right" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff _ _ m = setStack [] m


cardFirestrike :: Card
cardFirestrike = Card "Firestrike" "Hurt for 4 for each card to the right" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (4 * (length . getStack $ m)) (otherPlayer p) m


cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 2, return this card to your hand" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p c m = modHand (addToHand c) p (hurt 2 (otherPlayer p) m)


cardPotion :: Card
cardPotion = Card "Potion" "Heal for 7" "heart-bottle.svg" eff
  where
    eff :: CardEff
    eff p _ m = heal 7 p m


cardVampire :: Card
cardVampire = Card "Vampire" "Lifesteal for 5" "fangs.svg" eff
  where
    eff :: CardEff
    eff p _ m = lifesteal 5 (otherPlayer p) m


cardSuccubus :: Card
cardSuccubus = Card "Succubus" "Lifesteal for 3 for each card to the right" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p _ m = lifesteal (3 * (length . getStack $ m)) (otherPlayer p) m


cardReversal :: Card
cardReversal = Card "Reversal" "Reverse the order of cards to the right" "pocket-watch.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStack reverse m


cardReflect :: Card
cardReflect = Card "Reflect" "All cards to the right change owner" "shield-reflect.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStackAll reflectEff m
    reflectEff :: StackCard -> StackCard
    reflectEff (StackCard owner (Card name desc img cardEff)) =
      StackCard (otherPlayer owner) (Card name desc img cardEff)


cardProphecy :: Card
cardProphecy = Card "Prophecy" "Return all cards to the right to their owner's hand" "crystal-ball.svg" eff
  where
    eff :: CardEff
    eff _ _ m =
      (modHand (bounceAll PlayerA (getStack m)) PlayerA) .
        (modHand (bounceAll PlayerB (getStack m)) PlayerB) $
          setStack [] m
    bounceAll :: WhichPlayer -> Stack -> Hand -> Hand
    bounceAll w s h = take maxHandLength (h ++ (fmap getCard (filter (owner w) s)))
    owner :: WhichPlayer -> StackCard -> Bool
    owner PlayerA (StackCard PlayerA _) = True
    owner PlayerB (StackCard PlayerB _) = True
    owner _ _ = False
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card


cardSiren :: Card
cardSiren = Card "Siren" "Your opponent gets two cards that hurt them for 8 each" "harpy.svg" eff
  where
    eff :: CardEff
    eff p _ m = modHand (times 2 (addToHand cardSong)) (otherPlayer p) m
    cardSong :: Card
    cardSong = Card "Siren's Song" "Hurt yourself for 8" "love-song.svg" (\p _ -> hurt 8 p)


cardSickness :: Card
cardSickness = Card "Sickness" "Make all cards to the right's healing hurt instead" "bleeding-heart.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStackAll patchCard m
    patchCard :: StackCard -> StackCard
    patchCard (StackCard owner (Card name desc img cardEff)) =
      StackCard owner $
        Card ("Sick " <> name) (desc <> "; afflicted by sickness.") img $
          patchEff cardEff (\m -> (reverseHeal PlayerA m) . (reverseHeal PlayerB m))
    reverseHeal :: WhichPlayer -> Model -> Model -> Model
    reverseHeal which m1 m2 =
      hurt (max 0 (((getLife which m2) - (getLife which m1)) * 2)) which m2


cardOffering :: Card
cardOffering = Card "Offering" "Hurt yourself for 10, then draw two cards" "chalice-drops.svg" eff
  where
    eff :: CardEff
    eff p _ m = (drawCard p) . (drawCard p) . (hurt 10 p) $ m


cardGoatFlute :: Card
cardGoatFlute = Card "Goat Flute" "Both players get two useless goats" "pan-flute.svg" eff
  where
    eff :: CardEff
    eff p _ m =
      modHand (times 2 (addToHand cardGoat)) p $
        modHand (times 2 (addToHand cardGoat)) (otherPlayer p) m
    cardGoat :: Card
    cardGoat = Card "Goat" "A useless card" "goat.svg" (\_ _ m -> m)


cardConfound :: Card
cardConfound = Card "Confound" "Shuffle the order of cards to the right" "moebius-star.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStack (\s -> shuffle s (getGen m)) m


cardObscurer :: Card
cardObscurer = Card "Obscurer" "Hurt for 4 and obscure the next card your opponent draws" "orb-wand.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 4 (otherPlayer p) $ modDeckHead obs (otherPlayer p) m
    obs :: Card -> Card
    obs card = Card "???" "An obscured card" "sight-disabled.svg" (\p _ -> modStack ((:) (StackCard p card)))


cardZen :: Card
cardZen = Card "Zen" "Obscure your hand" "meditation.svg" eff
  where
    eff :: CardEff
    eff p _ m = modHand (fmap obs) p m
    obs :: Card -> Card
    obs card = Card "???" "An obscured card" "sight-disabled.svg" (\p _ -> modStack ((:) (StackCard p card)))


cardHammer :: Card
cardHammer = Card "Hammer" "Hurt for 6" "thor-hammer.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 6 (otherPlayer p) m


cardAgility :: Card
cardAgility = Card "Agility" "Draw a card" "sprint.svg" eff
  where
    eff :: CardEff
    eff p _ m = drawCard p m


cardCrossbow :: Card
cardCrossbow = Card "Crossbow" "Hurt for 9" "crossbow.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 9 (otherPlayer p) m


cardLightning :: Card
cardLightning = Card "Lightning" "Hurt for 3 for each card to the right" "lightning-branches.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (3 * (length . getStack $ m)) (otherPlayer p) m


cardStaff :: Card
cardStaff = Card "Staff" "Hurt for 4" "bo.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 4 (otherPlayer p) m


cardEcho :: Card
cardEcho = Card "Echo" "The next card to the right happens twice" "echo-ripples.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStackHead
      (\(StackCard which (Card name desc pic e)) ->
        StackCard which (Card name desc pic (\w c -> (e w c) . (e w c))))
      m


cardEnvy :: Card
cardEnvy = Card "Envy" "Hurt for 2 for each card in your opponent's hand" "mouth-watering.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (2 * (length . (getHand (otherPlayer p)) $ m)) (otherPlayer p) m
