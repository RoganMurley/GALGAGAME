module GodMode where

import Data.List (find)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import Card (Card(..))
import CardAnim (Hurt(..))
import Cards (allCards)
import Player (WhichPlayer(..), other)
import Util (Err, breakAt)
import User (User(..))

import qualified Auth.Schema as Auth
import qualified DSL.Beta as Beta


parse :: WhichPlayer -> Text -> Either Err (Beta.Program ())
parse which msg =
  let
    (command, content) = breakAt " " msg :: (Text, Text)
  in
    case command of
      "slash" ->
        case readMay $ cs content of
          Just d ->
            Right $ Beta.hurt d (other which) Slash
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      "heal" ->
        case readMay $ cs content of
          Just h ->
            Right $ Beta.heal h which
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      "draw" ->
        Right $ Beta.draw which
      "card" ->
        case find (\c -> card_name c == content) allCards of
          Just card ->
            Right $ Beta.addToHand which card
          Nothing ->
            Left ("Unknown card: " <> content :: Err)
      _ ->
        Left ("Unknown commandment: " <> command :: Err)


isSuperuser :: User -> Bool
isSuperuser (User user) = Auth.userSuperuser user
isSuperuser CpuUser     = False
isSuperuser GuestUser   = False
