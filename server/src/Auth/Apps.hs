module Auth.Apps where

import Prelude hiding (length)

import Config (App, getTokenConn, getUserConn)
import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (validatePassword)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.String.Conversions (cs)
import Web.Cookie (parseCookiesText)

import qualified Network.WebSockets as WS
import qualified Database.Redis as R

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
  conn <- getTokenConn
  gotten <- lift $ R.runRedis conn $ R.get $ T.encodeUtf8 token
  case gotten of
    Right username ->
      lift $ return $ T.decodeUtf8 <$> username
    Left _ -> do
      lift $ return Nothing


deleteToken :: Token -> App ()
deleteToken token = do
  conn <- getTokenConn
  _ <- lift $ R.runRedis conn $ R.del [T.encodeUtf8 token]
  return ()


usernameExists :: ByteString -> App DatabaseResult
usernameExists username = do
  conn <- getUserConn
  result <- lift $ R.runRedis conn $ R.exists username
  return $
    case result of
      Right True ->
        Found
      Right False ->
        NotFound
      Left err ->
        DatabaseError err


saveUser :: ByteString -> ByteString -> App ()
saveUser username hashedPassword = do
  conn <- getUserConn
  _ <- lift $ R.runRedis conn $ R.set username hashedPassword
  return ()


checkPassword :: ByteString -> ByteString -> App DatabaseResult
checkPassword username password = do
  userConn <- getUserConn
  result <- lift $ R.runRedis userConn $ R.get username
  return $
    case result of
      Right (Just hashedPassword) ->
        if validatePassword hashedPassword password then
          Found
        else
          NotFound
      Right Nothing ->
        NotFound
      Left err ->
        DatabaseError err


data DatabaseResult =
    Found
  | NotFound
  | DatabaseError R.Reply


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
