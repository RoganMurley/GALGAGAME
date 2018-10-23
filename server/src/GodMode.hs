module GodMode where

import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import Player (WhichPlayer(..), other)
import Util (Err, breakAt)

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
            Right $ Beta.slash d (other which)
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      "heal" ->
        case readMay $ cs content of
          Just h ->
            Right $ Beta.heal h which
          Nothing ->
            Left ("Cannot parse " <> content <> " to int" :: Err)
      _ ->
        Left ("Unknown commandment: " <> command :: Err)
