module Auth.Apps where

import Auth.Schema as Schema
import Config (App, runBeam, runBeamIntegrity, runRedis)
import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (validatePassword)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Database.Beam (all_, filter_, insert, insertValues, runInsert, runSelectReturningOne, select, val_, (==.))
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..))
import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Schema as Schema
import System.Log.Logger (errorM)
import Text.Printf (printf)
import Web.Cookie (parseCookiesText)
import Prelude hiding (length)

type Token = Text

type Username = Text

type Seconds = Integer

type Cookies = Map Text Text

saveSession :: ByteString -> Token -> App ()
saveSession username token = do
  let tokenBytestring = cs token :: ByteString
  runRedis $ do
    _ <- R.set tokenBytestring username
    return ()

checkAuth :: Maybe Token -> App (Maybe Username)
checkAuth Nothing = return Nothing
checkAuth (Just token) = do
  result <- runRedis $ R.get $ T.encodeUtf8 token
  case result of
    Right username ->
      return $ T.decodeUtf8 <$> username
    Left err -> do
      liftIO $ errorM "auth" $ printf "Database connection error %s" (show err)
      return $ Nothing

deleteToken :: Token -> App ()
deleteToken token = do
  _ <- runRedis $ R.del [T.encodeUtf8 token]
  return ()

saveUser :: ByteString -> ByteString -> ByteString -> Bool -> App Bool
saveUser email username hashedPassword contactable = do
  let user = Schema.User (cs email) (cs username) (cs hashedPassword) contactable False
  let stat = Schema.Stats (Schema.UserId $ cs username) 0
  userResult <- runBeamIntegrity $ runInsert $ insert (users galgagameDb) $ insertValues [user]
  statsResult <- runBeamIntegrity $ runInsert $ insert (stats galgagameDb) $ insertValues [stat]
  case userResult <* statsResult of
    Right _ ->
      return True
    Left (UniqueViolation "users_pkey") ->
      return False
    Left err ->
      throw err

checkPassword :: ByteString -> ByteString -> App Bool
checkPassword username password = do
  user <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Schema.userUsername row ==. val_ (cs username)) $
            all_ $ users galgagameDb
  return $
    case Schema.userPasshash <$> user of
      Just hashedPassword ->
        validatePassword (cs hashedPassword) password
      Nothing ->
        False

sessionCookieName :: Text
sessionCookieName = "login"

getCookies :: WS.PendingConnection -> Cookies
getCookies pending = Map.fromList cookiesList
  where
    cookieHeaders :: WS.Headers
    cookieHeaders = WS.requestHeaders . WS.pendingRequest $ pending
    cookieString :: Maybe ByteString
    cookieString = snd <$> find (((==) "Cookie") . fst) cookieHeaders
    cookiesList :: [(Text, Text)]
    cookiesList = case cookieString of
      Just str ->
        parseCookiesText str
      Nothing ->
        []

legalName :: ByteString -> Maybe Text
legalName n
  | length n < 3 = Just "Username too short"
  | length n > 12 = Just "Username too long"
  | otherwise = Nothing

legalPassword :: ByteString -> Maybe Text
legalPassword p
  | length p < 8 = Just "Password too short"
  | otherwise = Nothing
