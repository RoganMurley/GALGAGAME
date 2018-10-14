module CardAnim where

import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Life (Life)
import Model (Stack)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import StackCard (StackCard)


data CardAnim
  = Slash WhichPlayer Life
  | Heal WhichPlayer
  | Draw WhichPlayer
  | Bite WhichPlayer Life
  | Reflect
  | Reverse
  | Hubris Stack
  | Play WhichPlayer Card Int
  | Transmute StackCard StackCard
  | Mill WhichPlayer Card
  | GameEnd (Maybe WhichPlayer)
  | Rotate
  | Windup
  deriving (Show, Eq)


type SfxUrl = Text
type ShaderName = Text


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [ "player" .= w
    , "anim"  .= ("slash" :: Text, d)
    ]
  toJSON (Heal w) =
    object
    [ "player" .= w
    , "anim"  .= ("heal" :: Text)
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
  toJSON (Hubris s) =
    object
    [ "player" .= PlayerA
    , "anim"  .= ("hubris" :: Text, s)
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


instance Mirror CardAnim where
  mirror (Slash w d)       = Slash (other w) d
  mirror (Heal w)          = Heal  (other w)
  mirror (Draw w)          = Draw  (other w)
  mirror (Bite w d)        = Bite (other w) d
  mirror Reflect           = Reflect
  mirror Reverse           = Reverse
  mirror (Hubris s)        = Hubris (mirror <$> s)
  mirror (Play w c i)      = Play (other w) c i
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
  mirror (GameEnd w)       = GameEnd (other <$> w)
  mirror (Mill w c)        = Mill (other w) c
  mirror Rotate            = Rotate
  mirror Windup            = Windup
