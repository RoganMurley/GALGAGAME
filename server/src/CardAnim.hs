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


data CardAnim
  = Heal WhichPlayer Life
  | Draw WhichPlayer
  | Hurt WhichPlayer Life Hurt
  | Reflect
  | Reverse
  | Confound
  | Play WhichPlayer Card Int
  | Transmute StackCard StackCard Transmute
  | Mill WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  | Fabricate StackCard
  | Bounce [CardBounce]
  | Discard [CardDiscard]
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
  toJSON Reflect =
    object
    [ "name"   .= ("reflect" :: Text)
    , "player" .= PlayerA
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
  toJSON (Transmute ca cb t) =
    object
    [ "name"      .= ("transmute" :: Text)
    , "player"    .= PlayerA
    , "cardA"     .= ca
    , "cardB"     .= cb
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
  toJSON (Discard d) =
    object
    [ "name"    .= ("discard" :: Text)
    , "player"  .= PlayerA
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
  mirror Reflect             = Reflect
  mirror Confound            = Confound
  mirror Reverse             = Reverse

  mirror (Play w c i)        = Play (other w) c i
  mirror (Transmute ca cb t) = Transmute (mirror ca) (mirror cb) t
  mirror (GameEnd w)         = GameEnd (other <$> w)
  mirror (Mill w c)          = Mill (other w) c
  mirror Rotate              = Rotate
  mirror Windup              = Windup
  mirror (Fabricate c)       = Fabricate (mirror c)
  mirror (Bounce b)          = Bounce b
  mirror (Discard d)         = Discard d
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

data Transmute
  = TransmuteCard
  | TransmuteOwner
  deriving (Show, Eq)

instance ToJSON Transmute where
  toJSON TransmuteCard  = "transmuteCard"
  toJSON TransmuteOwner = "transmuteOwner"


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
