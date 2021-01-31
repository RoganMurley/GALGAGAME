module GodMode where

import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import CardAnim (Hurt(..))
import Cards (cardsByName)
import Player (WhichPlayer(..), other)
import Util (Err, breakAt)

import qualified Data.Map as Map
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
      "slashSelf" ->
        case readMay $ cs content of
          Just d ->
            Right $ Beta.hurt d which Slash
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      "heal" ->
        case readMay $ cs content of
          Just h ->
            Right $ Beta.heal h which
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      "draw" ->
        Right $ Beta.draw which which 0.1
      "card" ->
        case Map.lookup content cardsByName of
          Just card ->
            Right $ Beta.addToHand which card
          Nothing ->
            Left ("Unknown card: " <> content :: Err)
      "windup" ->
        Right $ Beta.windup
      "rotate" ->
        Right $ Beta.rotate
      _ ->
        Left ("Unknown commandment: " <> command :: Err)
