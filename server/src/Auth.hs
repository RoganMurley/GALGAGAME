module Auth where

import Prelude hiding (length)

import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson ((.=), object)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Time.Clock (secondsToDiffTime)
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Web.Cookie (parseCookiesText, sameSiteStrict, setCookieHttpOnly, setCookieMaxAge, setCookiePath, setCookieSameSite, setCookieSecure)
import Web.Scotty
import Web.Scotty.Cookie (deleteCookie, getCookie, makeSimpleCookie, setCookie)
import System.Log.Logger (Priority(DEBUG), debugM, errorM, setLevel, updateGlobalLogger)

import qualified Network.WebSockets as WS

import qualified Data.GUID as GUID
import qualified Database.Redis as R

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


app :: R.Connection -> R.Connection -> IO Application
app userConn tokenConn = do
  updateGlobalLogger "auth" $ setLevel DEBUG
  scottyApp $ do
    post "/auth/login"    $ login userConn tokenConn
    post "/auth/logout"   $ logout tokenConn
    post "/auth/register" $ register userConn


login :: R.Connection -> R.Connection -> ActionM ()
login userConn tokenConn = do
  usernameRaw <- param "username"
  password <- param "password"
  let username = (cs . T.toLower $ usernameRaw) :: ByteString
  gotten <- lift . (R.runRedis userConn) $ R.get username
  case gotten of
    Right pass ->
      case pass of
        Just hashedPassword ->
          case validatePassword hashedPassword password of
            True -> do
              token <- lift GUID.genText
              let cookie = (makeSimpleCookie "login" token) {
                  setCookieMaxAge = Just (secondsToDiffTime loginTimeout),
                  setCookieSecure = True,
                  setCookieHttpOnly = True,
                  setCookiePath = Just "/",
                  setCookieSameSite = Just sameSiteStrict
                }
              setCookie cookie
              _ <- lift . (R.runRedis tokenConn) $ do
                _ <- R.set (cs token) username
                R.expire (cs token) loginTimeout
              json $ object []
              status ok200
            False -> do
              json $ object [ "error" .= ("Wrong username or password" :: Text) ]
              status unauthorized401
        Nothing -> do
          json $ object [ "error" .= ("Wrong username or password" :: Text) ]
          status unauthorized401
    Left _ -> do
      lift $ errorM "auth" "Database connection error"
      json $ object [ "error" .= ("Database connection error" :: Text) ]
      status internalServerError500


logout :: R.Connection -> ActionM ()
logout tokenConn = do
  token <- getCookie "login"
  case token of
    Just t -> do
      _ <- lift . (R.runRedis tokenConn) $ R.del [T.encodeUtf8 t]
      deleteCookie "login"
      status noContent204
    Nothing ->
      status noContent204


register :: R.Connection -> ActionM ()
register userConn = do
  usernameRaw <- param "username"
  password <- param "password"
  let username = (cs . T.toLower $ usernameRaw) :: ByteString
  alreadyExists <- lift . (R.runRedis userConn) $ R.exists username
  case alreadyExists of
    Right False -> do
      case legalName username of
        Just err -> do
          lift $ debugM "auth" ("Registration: (" <> (cs username) <> ") " <> (cs err))
          json $ object [ "error" .= err ]
          status badRequest400
        Nothing -> do
          case legalPassword password of
            Just err -> do
              lift $ debugM "auth" ("Registration: (" <> (cs password) <> ") " <> (cs err))
              json $ object [ "error" .= err ]
              status badRequest400
            Nothing -> do
              p <- lift $ hashPasswordUsingPolicy slowerBcryptHashingPolicy password
              case p of
                Just hashedPassword -> do
                  _ <- lift . (R.runRedis userConn) $ R.set username hashedPassword
                  json $ object [ ]
                  status created201
                Nothing ->
                  status internalServerError500
    Right True -> do
      lift $ debugM "auth" ("Registration: user " <> (cs username) <> " already exists")
      json $ object [ "error" .= ("Username already exists" :: Text) ]
      status conflict409
    Left _ -> do
      lift $ errorM "auth" "Database connection error"
      json $ object [ "error" .= ("Database connection error" :: Text) ]
      status internalServerError500


checkAuth :: R.Connection -> Maybe Token -> IO (Maybe Username)
checkAuth _         Nothing      = return Nothing
checkAuth tokenConn (Just token) = do
  gotten <- (R.runRedis tokenConn) . R.get $ T.encodeUtf8 token
  case gotten of
    Right username ->
      return (T.decodeUtf8 <$> username)
    Left _ -> do
      errorM "auth" "Database connection error"
      return Nothing


type Token    = Text
type Username = Text
type Seconds  = Integer


data Database =
    UserDatabase
  | TokenDatabase


connectInfo :: Database -> R.ConnectInfo
connectInfo database =
  R.defaultConnectInfo
    { R.connectHost     = "redis"
    , R.connectDatabase = asId database
    }
  where
    asId :: Database -> Integer
    asId UserDatabase  = 0
    asId TokenDatabase = 1


loginTimeout :: Seconds
loginTimeout = 3600 * 24 * 7


getToken :: WS.PendingConnection -> Maybe Token
getToken pending = snd <$> find (((==) "login") . fst) cookies
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
