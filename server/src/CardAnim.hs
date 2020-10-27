module CardAnim where

import Bounce (CardBounce)
import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Discard (CardDiscard)
import Life (Life)
import Limbo (CardLimbo)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import StackCard (StackCard)
import Transmutation (Transmutation)


data CardAnim
  = Heal WhichPlayer Life
  | Draw WhichPlayer
  | Hurt WhichPlayer Life Hurt
  | Reverse
  | Confound
  | Play WhichPlayer Card Int
  | Transmute [Transmutation]
  | Mill WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  | Fabricate StackCard
  | Bounce [CardBounce]
  | DiscardStack [CardDiscard]
  | DiscardHand WhichPlayer [CardDiscard]
  | Pass WhichPlayer
  | Limbo [CardLimbo]
  | Unlimbo
  deriving (Show, Eq)


instance ToJSON CardAnim where
  toJSON (Hurt w d h) =
    object
    [ "name"   .= ("hurt" :: Text)
    , "player" .= w
    , "damage" .= d
    , "hurt"   .= h
    ]
  toJSON (Heal w h) =
    object
    [ "name"   .= ("heal" :: Text)
    , "player" .= w
    , "heal"   .= h
    ]
  toJSON (Draw w) =
    object
    [ "name"   .= ("draw" :: Text)
    , "player" .= w
    ]
  toJSON Confound =
    object
    [ "name"   .= ("confound" :: Text)
    , "player" .= PlayerA
    ]
  toJSON Reverse =
    object
    [ "name"   .= ("reverse" :: Text)
    , "player" .= PlayerA
    ]
  toJSON (Play w c i) =
    object
    [ "name"   .= ("play" :: Text)
    , "player" .= w
    , "card"   .= c
    , "index"  .= i
    ]
  toJSON (Transmute t) =
    object
    [ "name"      .= ("transmute" :: Text)
    , "player"    .= PlayerA
    , "transmute" .= t
    ]
  toJSON (Mill w c) =
    object
    [ "name"   .= ("mill" :: Text)
    , "player" .= w
    , "card"   .= c
    ]
  toJSON (GameEnd w) =
    object
    [ "name"   .= ("gameEnd" :: Text)
    , "player" .= PlayerA
    , "winner" .= w
    ]
  toJSON Rotate =
    object
    [ "name"   .= ("rotate" :: Text)
    , "player" .= PlayerA
    ]
  toJSON Windup =
    object
    [ "name"   .= ("windup" :: Text)
    , "player" .= PlayerA
    ]
  toJSON (Fabricate stackCard) =
    object
    [ "name"      .= ("fabricate" :: Text)
    , "player"    .= PlayerA
    , "stackCard" .= stackCard
    ]
  toJSON (Bounce b) =
    object
    [ "name"   .= ("bounce" :: Text)
    , "player" .= PlayerA
    , "bounce" .= b
    ]
  toJSON (DiscardStack d) =
    object
    [ "name"    .= ("discardStack" :: Text)
    , "player"  .= PlayerA
    , "discard" .= d
    ]
  toJSON (DiscardHand w d) =
    object
    [ "name"    .= ("discardHand" :: Text)
    , "player"  .= w
    , "discard" .= d
    ]
  toJSON (Pass w) =
    object
    [ "name"   .= ("pass" :: Text)
    , "player" .= w
    ]
  toJSON (Limbo l) =
    object
    [ "name"   .= ("limbo" :: Text)
    , "player" .= PlayerA
    , "limbo"  .= l
    ]
  toJSON Unlimbo =
    object
    [ "name"    .= ("unlimbo" :: Text)
    , "player"  .= PlayerA
    ]


instance Mirror CardAnim where
  mirror (Hurt w d h)        = Hurt (other w) d h
  mirror (Heal w h)          = Heal (other w) h
  mirror (Draw w)            = Draw  (other w)
  mirror Confound            = Confound
  mirror Reverse             = Reverse
  mirror (Play w c i)        = Play (other w) c i
  mirror (Transmute t)       = Transmute (mirror <$> t)
  mirror (GameEnd w)         = GameEnd (other <$> w)
  mirror (Mill w c)          = Mill (other w) c
  mirror Rotate              = Rotate
  mirror Windup              = Windup
  mirror (Fabricate c)       = Fabricate (mirror c)
  mirror (Bounce b)          = Bounce b
  mirror (DiscardStack d)    = DiscardStack d
  mirror (DiscardHand w d)   = DiscardHand (other w) d
  mirror (Pass w)            = Pass (other w)
  mirror (Limbo l)           = Limbo l
  mirror Unlimbo             = Unlimbo


data Hurt
  = Slash
  | Bite
  | Curse
  deriving (Show, Eq)

instance ToJSON Hurt where
  toJSON Slash = "slash"
  toJSON Bite  = "bite"
  toJSON Curse = "curse"


type SfxUrl = Text
type ShaderName = Text


cardAnimDamage :: CardAnim -> (Life, Life)
cardAnimDamage anim =
  let
      wrap :: WhichPlayer -> Life -> ( Life, Life )
      wrap w d =
          case w of
              PlayerA ->
                  ( d, 0 )

              PlayerB ->
                  ( 0, d )
  in
  case anim of
      Heal w h ->
          wrap w h

      Hurt w d _ ->
          wrap w (-d)

      _ ->
          ( 0, 0 )
