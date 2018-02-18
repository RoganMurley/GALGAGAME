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
  | Heal WhichPlayer
  | Draw WhichPlayer
  | Reverse
  | Obliterate
  | Play WhichPlayer Card
  | Transmute StackCard StackCard
  deriving (Show, Eq)


instance ToJSON CardAnim where
  toJSON (Slash w d) =
    object
    [
     "player" .= w
    , "anim"  .= ("slash" :: Text, d)
    ]
  toJSON (Heal w) =
    object
    [
     "player" .= w
    , "anim"  .= ("heal" :: Text)
    ]
  toJSON (Draw w) =
    object
    [
     "player" .= w
    , "anim"  .= ("draw" :: Text)
    ]
  toJSON Reverse =
    object
    [
     "player" .= PlayerA
    , "anim"  .= ("reverse" :: Text)
    ]
  toJSON Obliterate =
    object
    [
     "player" .= PlayerA
    , "anim"  .= ("obliterate" :: Text)
    ]
  toJSON (Play w c) =
    object
    [
     "player" .= w
    , "anim"  .= ("play" :: Text, c)
    ]
  toJSON (Transmute ca cb) =
    object
    [
     "player" .= PlayerA
    , "anim"  .= ("transmute" :: Text, ca, cb)
    ]


instance Mirror CardAnim where
  mirror (Slash w d)   = Slash (other w) d
  mirror (Heal w)      = Heal  (other w)
  mirror (Draw w)      = Draw  (other w)
  mirror Reverse       = Reverse
  mirror Obliterate    = Obliterate
  mirror (Play w c)    = Play (other w) c
  mirror (Transmute ca cb) = Transmute (mirror ca) (mirror cb)
