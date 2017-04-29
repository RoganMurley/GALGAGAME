module Cards where

import Model
import Util (shuffle, times)


-- Flame
cardDragon :: Card
cardDragon = Card "Dragon" "Hurt for 11" "dragon/dragon.svg" "dagger.wav" eff
  where
    eff :: CardEff
    eff p m = hurt 12 (otherPlayer p) m


cardFirestorm :: Card
cardFirestorm = Card "Firestorm" "Hurt for 5 for each card to the right" "dragon/fire-ray.svg" "fireball.wav" eff
  where
    eff :: CardEff
    eff p m = hurt (5 * (length . getStack $ m)) (otherPlayer p) m


cardOffering :: Card
cardOffering = Card "Offering" "Hurt yourself for 8, then draw two cards" "dragon/heartburn.svg" "offering.wav" eff
  where
    eff :: CardEff
    eff p m = (drawCard p) . (drawCard p) . (hurt 8 p) $ m


cardHaze :: Card
cardHaze = Card "Haze" "Shuffle the order of cards to the right" "dragon/heat-haze.svg" "confound.wav" eff
  where
    eff :: CardEff
    eff _ m = modStack (\s -> shuffle s (getGen m)) m


-- Thunder
cardStag :: Card
cardStag = Card "Stag" "Hurt for 10" "stag/stag.svg" "hammer.wav" eff
  where
    eff :: CardEff
    eff p m = hurt 10 (otherPlayer p) m


cardLightning :: Card
cardLightning = Card "Lightning" "Hurt for 4 for each card to the right" "stag/lightning-trio.svg" "lightning.wav" eff
  where
    eff :: CardEff
    eff p m = hurt (4 * (length . getStack $ m)) (otherPlayer p) m


cardHubris :: Card
cardHubris = Card "Hubris" "Negate all cards to the right" "stag/tower-fall.svg" "hubris.wav" eff
  where
    eff :: CardEff
    eff _ m = setStack [] m


cardEcho :: Card
cardEcho = Card "Echo" "The next card to the right's effect happens twice" "stag/echo-ripples.svg" "echo.wav" eff
  where
    eff :: CardEff
    eff _ m = modStackHead
      (\(StackCard which (Card name desc pic sfx e)) ->
        StackCard which (Card name desc pic sfx (\w -> (e w) . (e w))))
      m


-- Frost
cardGem :: Card
cardGem = Card "Powergem" "Hurt for 9" "gem/gem.svg" "axe.mp3" eff
  where
    eff :: CardEff
    eff p m = hurt 9 (otherPlayer p) m


cardBlizzard :: Card
cardBlizzard = Card "Blizzard" "Hurt the weakest player for 15" "gem/ice-spear.svg" "frostbite.mp3" eff
  where
    eff :: CardEff
    eff _ m
      | getLife PlayerA m < getLife PlayerB m =
        hurt dmg PlayerA $ m
      | getLife PlayerA m > getLife PlayerB m =
        hurt dmg PlayerB $ m
      | otherwise =
        (hurt dmg PlayerA) . (hurt dmg PlayerB) $ m
    dmg = 15


cardCrystal :: Card
cardCrystal = Card "Crystal" "Heal the weakest player for 15" "gem/crystal-growth.svg" "oath.wav" eff
  where
    eff :: CardEff
    eff _ m
      | getLife PlayerA m < getLife PlayerB m =
        heal mag PlayerA $ m
      | getLife PlayerA m > getLife PlayerB m =
        heal mag PlayerB $ m
      | otherwise =
        (heal mag PlayerA) . (heal mag PlayerB) $ m
    mag = 15


cardAlchemy :: Card
cardAlchemy = Card "Alchemy" "The next card to the right's effect becomes: draw 2 cards" "gem/alchemy.svg" "feint.wav" eff
  where
    eff :: CardEff
    eff _ m = modStackHead (\(StackCard w _) -> StackCard w c) m
    c :: Card
    c = Card "Gold" "Draw 2 cards" "gem/gold.svg" "feint.wav" (\p -> (drawCard p) . (drawCard p))


-- Tempest
cardOctopus :: Card
cardOctopus = Card "Octopus" "Lifesteal for 8" "octopus/octopus.svg" "bite.wav" eff
  where
    eff :: CardEff
    eff p m = lifesteal 8 (otherPlayer p) m


cardTentacles :: Card
cardTentacles = Card "Tentacles" "Lifesteal for 4 for each card to the right" "octopus/tentacle-strike.svg" "succubus.wav" eff
  where
    eff :: CardEff
    eff p m = lifesteal (4 * (length . getStack $ m)) (otherPlayer p) m


cardSiren :: Card
cardSiren = Card "Siren" "Your opponent gets two cards that hurt them for 8 each" "octopus/mermaid.svg" "siren.wav" eff
  where
    eff :: CardEff
    eff p m = modHand (times 2 (addToHand cardSong)) (otherPlayer p) m
    cardSong :: Card
    cardSong = Card "Siren's Song" "Hurt yourself for 8" "octopus/love-song.svg" "song.wav" (hurt 8)


cardReversal :: Card
cardReversal = Card "Reversal" "Reverse the order of cards to the right" "octopus/pocket-watch.svg" "reversal.wav" eff
  where
    eff :: CardEff
    eff _ m = modStack reverse m


-- Mist
cardMonkey :: Card
cardMonkey = Card "Monkey" "Hurt for 7" "monkey/monkey.svg" "shuriken.wav" eff
  where
    eff :: CardEff
    eff p m = hurt 7 (otherPlayer p) m

cardMonsoon :: Card
cardMonsoon = Card "Monsoon" "Hurt for 3 for each card in your hand" "monkey/heavy-rain.svg" "superego.wav" eff
  where
    eff :: CardEff
    eff p m = hurt (3 * (length . (getHand p) $ m)) (otherPlayer p) m


cardMindgate :: Card
cardMindgate = Card "Mindgate" "Your hand becomes the same as your opponent's" "monkey/magic-portal.svg" "mindgate.wav" eff
  where
    eff :: CardEff
    eff p m = setHand p (getHand (otherPlayer p) m) m


cardFeint :: Card
cardFeint = Card "Feint" "Return all of your cards to the right to your hand" "monkey/quick-slash.svg" "feint.wav" eff
  where
    eff :: CardEff
    eff p m =
      (modHand (bounceAll p (getStack m)) p) $
        modStack (filter (\(StackCard owner _) -> owner /= p)) m


-- Vortex
cardOwl :: Card
cardOwl = Card "Owl" "Hurt for 6" "owl/owl.svg" "staff.wav" eff
  where
    eff :: CardEff
    eff p m = hurt 6 (otherPlayer p) m


cardTwister :: Card
cardTwister = Card "Twister" "Hurt for 3 for each card in your opponent's hand" "owl/tornado.svg" "envy.wav" eff
  where
    eff :: CardEff
    eff p m = hurt (3 * (length . (getHand (otherPlayer p)) $ m)) (otherPlayer p) m


cardHypnosis :: Card
cardHypnosis = Card "Hypnosis" "Obscure your opponent's hand" "owl/vortex.svg" "mindhack.wav" eff
  where
    eff :: CardEff
    eff p m = modHand (fmap obs) (otherPlayer p) m
    obs :: Card -> Card
    obs card = Card "???" "An obscured card" "owl/sight-disabled.svg" "resolve.wav" (\p -> modStack ((:) (StackCard p card)))


cardProphecy :: Card
cardProphecy = Card "Prophecy" "Return all cards to the right to their owner's hand" "owl/star-pupil.svg" "precognition.wav" eff
  where
    eff :: CardEff
    eff _ m =
      (modHand (bounceAll PlayerA (getStack m)) PlayerA) .
        (modHand (bounceAll PlayerB (getStack m)) PlayerB) $
          setStack [] m


-- Calm
cardTurtle :: Card
cardTurtle = Card "Turtle" "Hurt for 5" "turtle/turtle.svg" "crossbow.wav" eff
  where
    eff :: CardEff
    eff p m = hurt 5 (otherPlayer p) m

cardGust :: Card
cardGust = Card "Gust" "Hurt for 3, return this card to your hand" "turtle/fluffy-cloud.svg" "boomerang.wav" eff
  where
    eff :: CardEff
    eff p m = modHand (addToHand cardGust) p (hurt 3 (otherPlayer p) m)


cardSoup :: Card
cardSoup = Card "Soup" "Heal for 10" "turtle/soup.svg" "potion.wav" eff
  where
    eff :: CardEff
    eff p m = heal 10 p m


cardReflect :: Card
cardReflect = Card "Reflect" "All cards to the right change owner" "turtle/shield-reflect.svg" "reflect.wav" eff
  where
    eff :: CardEff
    eff _ m = modStackAll reflectEff m
    reflectEff :: StackCard -> StackCard
    reflectEff (StackCard owner card) =
      StackCard (otherPlayer owner) card
