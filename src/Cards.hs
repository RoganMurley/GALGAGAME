module Cards where

import Data.Monoid ((<>))

import Model
import Util (shuffle, times)


cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 10" "plain-dagger.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 10 (otherPlayer p) m


cardHubris :: Card
cardHubris = Card "Hubris" "Negate all cards to the right" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff _ _ m = setStack [] m


cardFirestrike :: Card
cardFirestrike = Card "Firestrike" "Hurt for 5 for each card to the right" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (5 * (length . getStack $ m)) (otherPlayer p) m


cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 2, return this card to your hand" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p c m = modHand (addToHand c) p (hurt 2 (otherPlayer p) m)


cardPotion :: Card
cardPotion = Card "Potion" "Heal for 9" "heart-bottle.svg" eff
  where
    eff :: CardEff
    eff p _ m = heal 9 p m


cardVampire :: Card
cardVampire = Card "Vampire" "Lifesteal for 7" "fangs.svg" eff
  where
    eff :: CardEff
    eff p _ m = lifesteal 7 (otherPlayer p) m


cardSuccubus :: Card
cardSuccubus = Card "Succubus" "Lifesteal for 4 for each card to the right" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p _ m = lifesteal (4 * (length . getStack $ m)) (otherPlayer p) m


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


cardPrecognition :: Card
cardPrecognition = Card "Precognition" "Return all cards to the right to their owner's hand" "star-pupil.svg" eff
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
cardOffering = Card "Offering" "Hurt yourself for 8, then draw three cards" "chalice-drops.svg" eff
  where
    eff :: CardEff
    eff p _ m = (drawCard p p) . (drawCard p p) . (drawCard p p) . (hurt 8 p) $ m


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
cardObscurer = Card "Obscurer" "Hurt for 6 and obscure the next card your opponent draws" "orb-wand.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 6 (otherPlayer p) $ modDeckHead obs (otherPlayer p) m
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
cardHammer = Card "Hammer" "Hurt for 8" "thor-hammer.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 8 (otherPlayer p) m


cardAgility :: Card
cardAgility = Card "Agility" "Draw two cards" "sprint.svg" eff
  where
    eff :: CardEff
    eff p _ m = (drawCard p p) . (drawCard p p) $ m


cardCrossbow :: Card
cardCrossbow = Card "Crossbow" "Hurt for 12" "crossbow.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 12 (otherPlayer p) m


cardLightning :: Card
cardLightning = Card "Lightning" "Hurt for 4 for each card to the right" "lightning-trio.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (4 * (length . getStack $ m)) (otherPlayer p) m


cardStaff :: Card
cardStaff = Card "Staff" "Hurt for 6" "bo.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 6 (otherPlayer p) m


cardEcho :: Card
cardEcho = Card "Echo" "The next card to the right's effect happens twice" "echo-ripples.svg" eff
  where
    eff :: CardEff
    eff _ _ m = modStackHead
      (\(StackCard which (Card name desc pic e)) ->
        StackCard which (Card name desc pic (\w c -> (e w c) . (e w c))))
      m


cardEnvy :: Card
cardEnvy = Card "Envy" "Hurt for 3 for each card in your opponent's hand" "mouth-watering.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (3 * (length . (getHand (otherPlayer p)) $ m)) (otherPlayer p) m


cardEmpathy :: Card
cardEmpathy = Card "Empathy" "Draw two cards from your opponent's deck" "telepathy.svg" eff
  where
    eff :: CardEff
    eff p _ m = (drawCard (otherPlayer p) p) . (drawCard (otherPlayer p) p) $ m


cardMindgate :: Card
cardMindgate = Card "Mindgate" "Your hand becomes the same as your opponent's" "magic-portal.svg" eff
  where
    eff :: CardEff
    eff p _ m = setHand p (getHand (otherPlayer p) m) m


cardSuperego :: Card
cardSuperego = Card "Superego" "Hurt for 3 for each card in your hand" "deadly-strike.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt (3 * (length . (getHand p) $ m)) (otherPlayer p) m


cardShuriken :: Card
cardShuriken = Card "Shuriken" "Hurt for 7" "ninja-star.svg" eff
  where
    eff :: CardEff
    eff p _ m = hurt 7 (otherPlayer p) m


cardMindhack :: Card
cardMindhack = Card "Mindhack" "Obscure your opponent's hand" "vortex.svg" eff
  where
    eff :: CardEff
    eff p _ m = modHand (fmap obs) (otherPlayer p) m
    obs :: Card -> Card
    obs card = Card "???" "An obscured card" "sight-disabled.svg" (\p _ -> modStack ((:) (StackCard p card)))
