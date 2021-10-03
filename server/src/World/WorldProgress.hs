module World.WorldProgress where

import Card (cardName)
import Config (App, runBeam)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), eitherDecode, encode, object, withObject)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Beam ((==.), all_, filter_, insert, insertValues, runInsert, runSelectReturningOne, runUpdate, save, select, val_)
import World.Pos (Pos)
import World.WorldKey (WorldKey(..))
import Schema (GalgagameDb(..), galgagameDb)
import Text.Printf (printf)
import Util (Gen, genToSeed, getGen, randomChoice, seedToGen, split)

import qualified Auth.Schema
import qualified Card
import qualified Cards
import qualified Data.Set as Set
import qualified Log
import qualified World.Schema as Schema


data WorldProgress = WorldProgress
  { worldprogress_key          :: WorldKey
  , worldprogress_visited      :: Set WorldKey
  , worldprogress_visitedEdges :: [(Pos, Pos)]
  , worldprogress_deck         :: [Text]
  , worldprogress_decisionId   :: Maybe Text
  , worldprogress_roomId       :: Maybe Text
  , worldprogress_gen          :: Gen
  } deriving (Show)


instance ToJSON WorldProgress where
  toJSON (WorldProgress{ worldprogress_key, worldprogress_visited, worldprogress_visitedEdges, worldprogress_deck, worldprogress_decisionId, worldprogress_roomId, worldprogress_gen }) =
    object [
      "key"          .= worldprogress_key
    , "visited"      .= worldprogress_visited
    , "visitedEdges" .= worldprogress_visitedEdges
    , "deck"         .= worldprogress_deck
    , "decisionId"   .= worldprogress_decisionId
    , "roomId"       .= worldprogress_roomId
    , "seed"         .= genToSeed worldprogress_gen
    ]

instance FromJSON WorldProgress where
  parseJSON =
    withObject "WorldProgress" $
    \o -> WorldProgress
      <$> o .: "key"
      <*> o .: "visited"
      <*> o .: "visitedEdges"
      <*> o .: "deck"
      <*> o .: "decisionId"
      <*> o .: "roomId"
      <*> (seedToGen <$> o .: "seed")


getInitialPairing :: Gen -> (Card.Aspect, Card.Aspect)
getInitialPairing gen = (randomChoice genA aspects, randomChoice genB aspects)
  where
    (genA, genB) = split gen
    aspects :: [Card.Aspect]
    aspects =
      [ Card.Heaven
      , Card.Tide
      , Card.Shroom
      , Card.Blaze
      ]


initialProgress :: Gen -> WorldProgress
initialProgress gen =
  let
    (aspect, _) = getInitialPairing gen
    initialCards = Cards.getAspectCards aspect
  in
    WorldProgress
      Start
      Set.empty
      []
      (cardName <$> initialCards)
      Nothing
      Nothing
      gen


updateProgress :: Maybe Text -> WorldProgress -> App ()
updateProgress (Just username) progressState = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Schema.progressUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ progress galgagameDb
  let prog = Schema.Progress (Auth.Schema.UserId $ cs username) (cs $ encode progressState)
  case result of
    Just _ -> do
      Log.info $ printf "World progress found, updating..."
      runBeam $ runUpdate $ save (progress galgagameDb) prog
    Nothing -> do
      Log.info $ printf "World progress not found, inserting..."
      runBeam $ runInsert $ insert (progress galgagameDb) $ insertValues [ prog ]
updateProgress Nothing _ = return ()


loadProgress :: Maybe Text -> App WorldProgress
loadProgress Nothing         = do
  gen <- liftIO getGen
  return $ initialProgress gen
loadProgress (Just username) = do
  result <- runBeam $ runSelectReturningOne $
    select $ filter_ (\row -> Schema.progressUser row ==. val_ (Auth.Schema.UserId username)) $
      all_ $ progress galgagameDb
  let progressState = Schema.progressState <$> result
  Log.info $ printf "progressState: %s" (show progressState)
  case progressState of
    Just state -> do
      let decoded = eitherDecode $ cs state :: Either String WorldProgress
      case decoded of
        Left err -> do
          Log.error $ printf "Error loading world progress: %s" err
          gen <- liftIO getGen
          return $ initialProgress gen
        Right progress ->
          return progress
    Nothing -> do
      gen <- liftIO getGen
      return $ initialProgress gen


refreshProgress :: WorldProgress -> WorldProgress
refreshProgress progress =
  if forceReset then
    initialProgress worldprogress_gen
  else
    progress { worldprogress_roomId = Nothing }
  where
    (WorldProgress{ worldprogress_roomId, worldprogress_gen }) = progress
    -- If the user is in a room force reset the progress
    forceReset :: Bool
    forceReset = isJust worldprogress_roomId


nextGen :: WorldProgress -> Gen
nextGen (WorldProgress{ worldprogress_gen }) = fst $ split $ worldprogress_gen
