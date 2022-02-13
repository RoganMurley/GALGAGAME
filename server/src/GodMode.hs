module GodMode where

import CardAnim (Hurt (..))
import Cards (cardsByName)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import HandCard (HandCard (..), hide)
import Player (WhichPlayer (..), other)
import Safe (readMay)
import Util (Err, breakAt)

data Parsed
  = ParsedProgram (Beta.Program ())
  | ParsedTimeLimit Int
  | ParsedXp Int
  | ParseError Err

parse :: WhichPlayer -> Text -> Parsed
parse which msg =
  let (command, content) = breakAt " " msg :: (Text, Text)
   in case command of
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
          ParsedProgram $ Beta.reveal (other which) (const . const $ True)
        "hide" ->
          ParsedProgram $ do
            Beta.raw $ Alpha.modHand (other which) (fmap hide)
            Beta.null
        "xp" ->
          case readMay $ cs content of
            Just xp ->
              ParsedXp xp
            Nothing ->
              ParseError ("Cannot parse " <> content <> " to int" :: Err)
        _ ->
          ParseError ("Unknown commandment: " <> command :: Err)
