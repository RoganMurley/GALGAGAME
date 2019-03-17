module CardAnim where

import Bounce (CardBounce)
import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Life (Life)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import StackCard (StackCard)


data CardAnim
  = Slash WhichPlayer Life
  | Heal WhichPlayer Life
  | Draw WhichPlayer
  | Bite WhichPlayer Life
  | Reflect
  | Reverse
  | Hubris
  | Play WhichPlayer Card Int
  | Transmute StackCard StackCard
  | Mill WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  | Fabricate StackCard
  | Bounce [CardBounce]
  | Pass WhichPlayer
  deriving (Show, Eq)


type SfxUrl = Text
type ShaderName = Text


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [ "name"   .= ("slash" :: Text)
    , "player" .= w
    , "damage" .= d
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
  toJSON (Bite w d) =
    object
    [ "name"   .= ("bite" :: Text)
    , "player" .= w
    , "damage" .= d
    ]
  toJSON Reflect =
    object
    [ "name"   .= ("reflect" :: Text)
    , "player" .= PlayerA
    ]
  toJSON Reverse =
    object
    [ "name"   .= ("reverse" :: Text)
    , "player" .= PlayerA
    ]
  toJSON Hubris =
    object
    [ "name"   .= ("hubris" :: Text)
    , "player" .= PlayerA
    ]
  toJSON (Play w c i) =
    object
    [ "name"   .= ("play" :: Text)
    , "player" .= w
    , "card"   .= c
    , "index"  .= i
    ]
  toJSON (Transmute ca cb) =
    object
    [ "name"   .= ("transmute" :: Text)
    , "player" .= PlayerA
    , "cardA"  .= ca
    , "cardB"  .= cb
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
  toJSON (Pass w) =
    object
    [ "name"   .= ("pass" :: Text)
    , "player" .= w
    ]


instance Mirror CardAnim where
  mirror (Slash w d)       = Slash (other w) d
  mirror (Heal w h)        = Heal (other w) h
  mirror (Draw w)          = Draw  (other w)
  mirror (Bite w d)        = Bite (other w) d
  mirror Reflect           = Reflect
  mirror Reverse           = Reverse
  mirror Hubris            = Hubris
  mirror (Play w c i)      = Play (other w) c i
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
  mirror (GameEnd w)       = GameEnd (other <$> w)
  mirror (Mill w c)        = Mill (other w) c
  mirror Rotate            = Rotate
  mirror Windup            = Windup
  mirror (Fabricate c)     = Fabricate (mirror c)
  mirror (Bounce b)        = Bounce b
  mirror (Pass w)   = Pass (other w)


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

      Slash w d ->
          wrap w (-d)

      Bite w d ->
          wrap w (-d)

      _ ->
          ( 0, 0 )
