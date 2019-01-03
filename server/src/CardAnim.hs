module CardAnim where

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
  | Bounce [Bool]
  deriving (Show, Eq)


type SfxUrl = Text
type ShaderName = Text


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [ "player" .= w
    , "anim"  .= ("slash" :: Text, d)
    ]
  toJSON (Heal w h) =
    object
    [ "player" .= w
    , "anim"  .= ("heal" :: Text, h)
    ]
  toJSON (Draw w) =
    object
    [ "player" .= w
    , "anim"  .= ("draw" :: Text)
    ]
  toJSON (Bite w d) =
    object
    [ "player" .= w
    , "anim"  .= ("bite" :: Text, d)
    ]
  toJSON Reflect =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("reflect" :: Text)
    ]
  toJSON Reverse =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("reverse" :: Text)
    ]
  toJSON Hubris =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("hubris" :: Text)
    ]
  toJSON (Play w c i) =
    object
    [ "player" .= w
    , "anim"  .= ("play" :: Text, c, i)
    ]
  toJSON (Transmute ca cb) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("transmute" :: Text, ca, cb)
    ]
  toJSON (Mill w c) =
    object
    [ "player" .= w
    , "anim"  .= ("mill" :: Text, c)
    ]
  toJSON (GameEnd w) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("gameEnd" :: Text, w)
    ]
  toJSON Rotate =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("rotate" :: Text)
    ]
  toJSON Windup =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("windup" :: Text)
    ]
  toJSON (Fabricate stackCard) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("fabricate" :: Text, stackCard)
    ]
  toJSON (Bounce l) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("bounce" :: Text, l)
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
  mirror (Bounce l)        = Bounce l
