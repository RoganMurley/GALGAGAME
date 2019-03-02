module Auth.Apps where

import Prelude hiding (length)

import Config (App, getTokenConn)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.String.Conversions (cs)
import Web.Cookie (parseCookiesText)

import qualified Network.WebSockets as WS
import qualified Database.Redis as R

import Data.Text (Text)
import qualified Data.Text.Encoding as T


saveSession :: ByteString -> Text -> App ()
saveSession username tokenText = do
  conn <- getTokenConn
  let token = cs tokenText :: ByteString
  lift $ R.runRedis conn $ do
    _ <- R.set token username
    _ <- R.expire token loginTimeout
    return ()


checkAuth :: Maybe Token -> App (Maybe Username)
checkAuth Nothing      = return Nothing
checkAuth (Just token) = do
  conn <- getTokenConn
  gotten <- lift $ (R.runRedis conn) . R.get $ T.encodeUtf8 token
  case gotten of
    Right username ->
      lift $ return (T.decodeUtf8 <$> username)
    Left _ -> do
      lift $ return Nothing


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
