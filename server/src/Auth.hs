module Auth where

import Prelude hiding (length)

import Config (App, ConnectInfoConfig(..), Config(..), getConfig, getTokenConn, runApp)
import Control.Monad.IO.Class (liftIO)
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


app :: ConnectInfoConfig -> App Application
app connectInfoConfig = do
  liftIO $ updateGlobalLogger "auth" $ setLevel DEBUG
  config <- getConfig
  liftIO $ scottyApp $ do
    get  "/auth/me"       $ meView       connectInfoConfig
    post "/auth/login"    $ loginView    config
    post "/auth/logout"   $ logoutView   config
    post "/auth/register" $ registerView config


meView :: ConnectInfoConfig -> ActionM ()
meView connectInfoConfig = do
  token     <- getCookie loginCookieName
  usernameM <- lift . runApp connectInfoConfig $ checkAuth token
  case usernameM of
    Just username -> do
      json $ object [ "username" .= username ]
      status ok200
    Nothing -> do
      json $ object [ ]
      status ok200


loginView :: Config -> ActionM ()
loginView config = do
  let userConn = Config.userConn config
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
              createSession config username
              json $ object [ ]
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


logoutView :: Config -> ActionM ()
logoutView config = do
  let tokenConn = Config.tokenConn config
  token <- getCookie loginCookieName
  case token of
    Just t -> do
      _ <- lift . (R.runRedis tokenConn) $ R.del [T.encodeUtf8 t]
      deleteCookie loginCookieName
      json $ object [ ]
      status ok200
    Nothing -> do
      json $ object [ ]
      status ok200


registerView :: Config -> ActionM ()
registerView config = do
  let conn = userConn config
  usernameRaw <- param "username"
  password <- param "password"
  let username = (cs . T.toLower $ usernameRaw) :: ByteString
  alreadyExists <- lift . (R.runRedis conn) $ R.exists username
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
                  _ <- lift . (R.runRedis conn) $ R.set username hashedPassword
                  createSession config username
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


createSession :: Config -> ByteString -> ActionM ()
createSession config username = do
  let tokenConn = Config.tokenConn config
  token <- lift GUID.genText
  let cookie = (makeSimpleCookie loginCookieName token) {
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
  return ()


checkAuth :: Maybe Token -> App (Maybe Username)
checkAuth Nothing      = return Nothing
checkAuth (Just token) = do
  conn <- getTokenConn
  gotten <- liftIO $ (R.runRedis conn) . R.get $ T.encodeUtf8 token
  case gotten of
    Right username ->
      liftIO $ return (T.decodeUtf8 <$> username)
    Left _ -> do
      liftIO $ return Nothing


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
