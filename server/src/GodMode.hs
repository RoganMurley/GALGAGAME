module GodMode where

import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (readMay)

import CardAnim (Hurt(..))
import Cards (cardsByName)
import HandCard (HandCard(..), reveal)
import Player (WhichPlayer(..), other)
import Util (Err, breakAt)

import qualified Data.Map as Map
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta


data Parsed = ParsedProgram (Beta.Program ())
            | ParsedTimeLimit Int
            | ParseError Err


parse :: WhichPlayer -> Text -> Parsed
parse which msg =
  let
    (command, content) = breakAt " " msg :: (Text, Text)
  in
    case command of
      "slash" ->
        case readMay $ cs content of
          Just d ->
            ParsedProgram $ Beta.hurt d (other which) Slash
          Nothing ->
            ParseError ("Cannot parse " <> content <> " to int" :: Err)
      "slashSelf" ->
        case readMay $ cs content of
          Just d ->
            ParsedProgram $ Beta.hurt d which Slash
          Nothing ->
            ParseError ("Cannot parse " <> content <> " to int" :: Err)
      "heal" ->
        case readMay $ cs content of
          Just h ->
            ParsedProgram $ Beta.heal h which
          Nothing ->
            ParseError ("Cannot parse " <> content <> " to int" :: Err)
      "draw" ->
        ParsedProgram $ Beta.draw which which 0.1
      "card" ->
        case Map.lookup content cardsByName of
          Just card ->
            ParsedProgram $ Beta.addToHand which (HandCard card)
          Nothing ->
            ParseError ("Unknown card: " <> content :: Err)
      "windup" ->
        ParsedProgram $ Beta.windup
      "rotate" ->
        ParsedProgram $ Beta.rotate
      "timeLimit" ->
        case readMay $ cs content of
          Just t ->
            ParsedTimeLimit t
          Nothing ->
            ParseError ("Cannot parse " <> content <> " to int" :: Err)
      "reveal" ->
        ParsedProgram $ do
          Beta.raw $ Alpha.modHand (other which) (fmap reveal)
          Beta.null
      _ ->
        ParseError ("Unknown commandment: " <> command :: Err)
