module Auth.Apps where

import Prelude hiding (length)

import Config (App, getTokenConn, getUserConn)
import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (validatePassword)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.String.Conversions (cs)
import Web.Cookie (parseCookiesText)
import Safe (headMay)
import System.Log.Logger (errorM)
import Text.Printf (printf)

import qualified Network.WebSockets as WS
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.Redis as R
import qualified Prelude

import Data.Text (Text)
import qualified Data.Text.Encoding as T


saveSession :: ByteString -> Token -> App ()
saveSession username token = do
  conn <- getTokenConn
  let tokenBytestring = cs token :: ByteString
  lift $ R.runRedis conn $ do
    _ <- R.set tokenBytestring username
    _ <- R.expire tokenBytestring loginTimeout
    return ()


checkAuth :: Maybe Token -> App (Maybe Username)
checkAuth Nothing      = return Nothing
checkAuth (Just token) = do
  conn   <- getTokenConn
  result <- lift $ R.runRedis conn $ R.get $ T.encodeUtf8 token
  case result of
    Right username ->
      return $ T.decodeUtf8 <$> username
    Left err -> do
      lift $ errorM "auth" $ printf "Database connection error %s" (show err)
      return $ Nothing


deleteToken :: Token -> App ()
deleteToken token = do
  conn <- getTokenConn
  _ <- lift $ R.runRedis conn $ R.del [T.encodeUtf8 token]
  return ()


usernameExists :: ByteString -> App Bool
usernameExists username = do
  conn <- getUserConn
  result <- lift $
    Postgres.query conn
      "SELECT user FROM users WHERE username=? LIMIT 1"
      (Postgres.Only username) :: App [Postgres.Only Text]
  return $ Prelude.length result > 0


saveUser :: ByteString -> ByteString -> ByteString -> App ()
saveUser email username hashedPassword = do
  conn <- getUserConn
  _ <- lift $
    Postgres.execute conn
      "INSERT INTO users (email, username, passhash) VALUES (?, ?, ?)"
      (email, username, hashedPassword)
  return ()


checkPassword :: ByteString -> ByteString -> App Bool
checkPassword username password = do
  conn <- getUserConn
  result <- lift $
    Postgres.query conn
      "SELECT passhash FROM users WHERE username=? LIMIT 1"
      (Postgres.Only $ username) :: App [Postgres.Only Text]
  return $
    case Postgres.fromOnly <$> headMay result of
      Just hashedPassword ->
        if validatePassword (cs hashedPassword) password then
          True
        else
          False
      Nothing ->
        False


type Token    = Text
type Username = Text
type Seconds  = Integer


loginCookieName :: Text
loginCookieName = "login"


loginTimeout :: Seconds
loginTimeout = 3600 * 24 * 7


getToken :: WS.PendingConnection -> Maybe Token
getToken pending = snd <$> find (((==) loginCookieName) . fst) cookies
  where
    cookies :: [(Text, Text)]
    cookies = case cookieString of
      Just str ->
        parseCookiesText str
      Nothing ->
        []
    cookieString :: Maybe ByteString
    cookieString = snd <$> find (((==) "Cookie") . fst) cookieHeaders
    cookieHeaders :: WS.Headers
    cookieHeaders = WS.requestHeaders . WS.pendingRequest $ pending


legalName :: ByteString -> Maybe Text
legalName n
  | length n < 3  = Just "Username too short"
  | length n > 12 = Just "Username too long"
  | otherwise     = Nothing


legalPassword :: ByteString -> Maybe Text
legalPassword p
  | length p < 8  = Just "Password too short"
  | otherwise     = Nothing
