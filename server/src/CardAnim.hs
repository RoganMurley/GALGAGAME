{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CardAnim where

import Bounce (CardBounce)
import Card (Card (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Discard (CardDiscard)
import GHC.Generics (Generic)
import HandCard (HandCard)
import Life (Life)
import Mirror (Mirror (..))
import Player (WhichPlayer (..), other)
import Transmutation (Transmutation)
import Wheel (Wheel)

data CardAnim
  = Heal WhichPlayer Life
  | Draw WhichPlayer TimeModifier
  | Hurt WhichPlayer Life Hurt
  | Play WhichPlayer HandCard Int
  | Transmute (Wheel (Maybe Transmutation))
  | Mill WhichPlayer Card TimeModifier
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  | Bounce (Wheel (Maybe CardBounce)) TimeModifier
  | BounceDeck (Wheel Bool) TimeModifier
  | DiscardStack (Wheel Bool)
  | DiscardHand WhichPlayer [CardDiscard]
  | MoveStack (Wheel (Maybe Int)) TimeModifier
  | Pass WhichPlayer
  | Reveal WhichPlayer [Bool]
  | RevealDeck WhichPlayer Card
  | Timeout
  | Announce Text TimeModifier
  | Tricked
  | GetGen
  | UnknownDamage
  deriving (Show, Eq, Generic, NFData)

instance ToJSON CardAnim where
  toJSON (Hurt w d h) =
    object
      [ "name" .= ("hurt" :: Text),
        "player" .= w,
        "damage" .= d,
        "hurt" .= h
      ]
  toJSON (Heal w h) =
    object
      [ "name" .= ("heal" :: Text),
        "player" .= w,
        "heal" .= h
      ]
  toJSON (Draw w t) =
    object
      [ "name" .= ("draw" :: Text),
        "timeModifier" .= t,
        "player" .= w
      ]
  toJSON (Play w c i) =
    object
      [ "name" .= ("play" :: Text),
        "player" .= w,
        "card" .= c,
        "index" .= i
      ]
  toJSON (Transmute t) =
    object
      [ "name" .= ("transmute" :: Text),
        "player" .= PlayerA,
        "transmute" .= t
      ]
  toJSON (Mill w c t) =
    object
      [ "name" .= ("mill" :: Text),
        "player" .= w,
        "card" .= c,
        "timeModifier" .= t
      ]
  toJSON (GameEnd w) =
    object
      [ "name" .= ("gameEnd" :: Text),
        "player" .= PlayerA,
        "winner" .= w
      ]
  toJSON Rotate =
    object
      [ "name" .= ("rotate" :: Text),
        "player" .= PlayerA
      ]
  toJSON Windup =
    object
      [ "name" .= ("windup" :: Text),
        "player" .= PlayerA
      ]
  toJSON (Bounce b t) =
    object
      [ "name" .= ("bounce" :: Text),
        "player" .= PlayerA,
        "bounce" .= b,
        "timeModifier" .= t
      ]
  toJSON (BounceDeck b t) =
    object
      [ "name" .= ("bounceDeck" :: Text),
        "player" .= PlayerA,
        "bounce" .= b,
        "timeModifier" .= t
      ]
  toJSON (DiscardStack d) =
    object
      [ "name" .= ("discardStack" :: Text),
        "player" .= PlayerA,
        "discard" .= d
      ]
  toJSON (DiscardHand w d) =
    object
      [ "name" .= ("discardHand" :: Text),
        "player" .= w,
        "discard" .= d
      ]
  toJSON (MoveStack m t) =
    object
      [ "name" .= ("moveStack" :: Text),
        "player" .= PlayerA,
        "moves" .= m,
        "time" .= t
      ]
  toJSON (Pass w) =
    object
      [ "name" .= ("pass" :: Text),
        "player" .= w
      ]
  toJSON (Reveal w r) =
    object
      [ "name" .= ("reveal" :: Text),
        "player" .= w,
        "reveal" .= r
      ]
  toJSON (RevealDeck w c) =
    object
      [ "name" .= ("revealDeck" :: Text),
        "card" .= c,
        "player" .= w
      ]
  toJSON GetGen =
    object
      [ "name" .= ("getGen" :: Text),
        "player" .= PlayerA
      ]
  toJSON UnknownDamage =
    object
      [ "name" .= ("unknownDamage" :: Text),
        "player" .= PlayerA
      ]
  toJSON Timeout =
    object
      [ "name" .= ("timeout" :: Text),
        "player" .= PlayerA
      ]
  toJSON (Announce a t) =
    object
      [ "name" .= ("announce" :: Text),
        "text" .= a,
        "time" .= t,
        "player" .= PlayerA
      ]
  toJSON Tricked =
    object
      [ "name" .= ("tricked" :: Text),
        "player" .= PlayerA
      ]

instance Mirror CardAnim where
  mirror (Hurt w d h) = Hurt (other w) d h
  mirror (Heal w h) = Heal (other w) h
  mirror (Draw w t) = Draw (other w) t
  mirror (Play w c i) = Play (other w) c i
  mirror (Transmute t) = Transmute (mirror <$> t)
  mirror (GameEnd w) = GameEnd (other <$> w)
  mirror (Mill w c t) = Mill (other w) c t
  mirror Rotate = Rotate
  mirror Windup = Windup
  mirror (Bounce b t) = Bounce b t
  mirror (BounceDeck b t) = BounceDeck b t
  mirror (DiscardStack d) = DiscardStack d
  mirror (DiscardHand w d) = DiscardHand (other w) d
  mirror (Reveal w r) = Reveal (other w) r
  mirror (RevealDeck w c) = RevealDeck (other w) (mirror c)
  mirror (MoveStack m t) = MoveStack m t
  mirror (Pass w) = Pass (other w)
  mirror GetGen = GetGen
  mirror UnknownDamage = UnknownDamage
  mirror Timeout = Timeout
  mirror (Announce a t) = Announce a t
  mirror Tricked = Tricked

data Hurt
  = Slash
  | Bite
  | Curse
  deriving (Eq, Generic, NFData, Show)

instance ToJSON Hurt where
  toJSON Slash = "slash"
  toJSON Bite = "bite"
  toJSON Curse = "curse"

type SfxUrl = Text

type ShaderName = Text

data Damage = DamageCertain Life | DamageUncertain Life
  deriving (Eq, Generic, NFData, Show)

data TimeModifier
  = TimeModifierLinear Float
  | TimeModifierOutQuad Float
  | TimeModifierOutQuint Float
  deriving (Eq, Generic, NFData, Show)

instance ToJSON TimeModifier where
  toJSON (TimeModifierLinear t) =
    object
      [ "ease" .= ("linear" :: Text),
        "t" .= t
      ]
  toJSON (TimeModifierOutQuint t) =
    object
      [ "ease" .= ("outQuint" :: Text),
        "t" .= t
      ]
  toJSON (TimeModifierOutQuad t) =
    object
      [ "ease" .= ("outQuad" :: Text),
        "t" .= t
      ]

instance ToJSON Damage where
  toJSON (DamageCertain a) = toJSON a
  toJSON (DamageUncertain a) = toJSON a

instance Num Damage where
  (+) (DamageCertain a) (DamageCertain b) = DamageCertain (a + b)
  (+) (DamageCertain a) (DamageUncertain b) = DamageUncertain (a + b)
  (+) (DamageUncertain a) (DamageCertain b) = DamageUncertain (a + b)
  (+) (DamageUncertain a) (DamageUncertain b) = DamageUncertain (a + b)

  (*) (DamageCertain a) (DamageCertain b) = DamageCertain (a * b)
  (*) (DamageCertain a) (DamageUncertain b) = DamageUncertain (a * b)
  (*) (DamageUncertain a) (DamageCertain b) = DamageUncertain (a * b)
  (*) (DamageUncertain a) (DamageUncertain b) = DamageUncertain (a * b)

  abs (DamageCertain a) = DamageCertain (abs a)
  abs (DamageUncertain a) = DamageUncertain (abs a)

  signum (DamageCertain a) = DamageCertain (signum a)
  signum (DamageUncertain a) = DamageUncertain (signum a)

  negate (DamageCertain a) = DamageCertain (negate a)
  negate (DamageUncertain a) = DamageUncertain (negate a)

  fromInteger a = DamageCertain (fromIntegral a)

cardAnimDamage :: CardAnim -> (Damage, Damage)
cardAnimDamage anim =
  let wrap :: WhichPlayer -> Life -> (Damage, Damage)
      wrap w d =
        case w of
          PlayerA ->
            (DamageCertain d, DamageCertain 0)
          PlayerB ->
            (DamageCertain 0, DamageCertain d)
   in case anim of
        Heal w h ->
          wrap w h
        Hurt w d _ ->
          wrap w (- d)
        UnknownDamage ->
          (DamageUncertain 0, DamageUncertain 0)
        _ ->
          (DamageCertain 0, DamageCertain 0)
