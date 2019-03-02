module Auth.Views where

import Config (App, ConnectInfoConfig(..), getTokenConn, getUserConn, runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson ((.=), object)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Time.Clock (secondsToDiffTime)
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Web.Cookie (sameSiteStrict, setCookieHttpOnly, setCookieMaxAge, setCookiePath, setCookieSameSite, setCookieSecure)
import Web.Scotty
import Web.Scotty.Cookie (deleteCookie, getCookie, makeSimpleCookie, setCookie)
import System.Log.Logger (Priority(DEBUG), debugM, errorM, setLevel, updateGlobalLogger)

import qualified Data.GUID as GUID
import qualified Database.Redis as R

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Auth.Apps (checkAuth, legalName, legalPassword, loginCookieName, loginTimeout, saveSession)


app :: ConnectInfoConfig -> App Application
app connectInfoConfig = do
  liftIO $ updateGlobalLogger "auth" $ setLevel DEBUG
  liftIO $ scottyApp $ do
    get  "/auth/me"       $ meView       connectInfoConfig
    post "/auth/login"    $ loginView    connectInfoConfig
    post "/auth/logout"   $ logoutView   connectInfoConfig
    post "/auth/register" $ registerView connectInfoConfig


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


loginView :: ConnectInfoConfig -> ActionM ()
loginView connectInfoConfig =
  let
    getPassword :: ByteString -> ActionM (Either R.Reply (Maybe ByteString))
    getPassword username =
      lift $ runApp connectInfoConfig $ do
        userConn <- getUserConn
        lift $ R.runRedis userConn $ R.get username
  in
    do
      usernameRaw <- param "username"
      password <- param "password"
      let username = cs . T.toLower $ usernameRaw
      mPass <- getPassword username
      case mPass of
        Right pass ->
          case pass of
            Just hashedPassword ->
              case validatePassword hashedPassword password of
                True -> do
                  createSession connectInfoConfig username
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


logoutView :: ConnectInfoConfig -> ActionM ()
logoutView connectInfoConfig =
  let
    deleteToken :: Text -> App ()
    deleteToken token = do
      tokenConn <- getTokenConn
      _ <- lift $ R.runRedis tokenConn $ R.del [T.encodeUtf8 token]
      return ()
  in
    do
      token <- getCookie loginCookieName
      case token of
        Just t -> do
          lift $ runApp connectInfoConfig $ deleteToken t
          deleteCookie loginCookieName
          json $ object [ ]
          status ok200
        Nothing -> do
          json $ object [ ]
          status ok200


registerView :: ConnectInfoConfig -> ActionM ()
registerView connectInfoConfig =
  let
    usernameExists :: ByteString -> ActionM (Either R.Reply Bool)
    usernameExists username =
      lift $ runApp connectInfoConfig $ do
        conn <- getUserConn
        lift $ R.runRedis conn $ R.exists username
    saveUser :: ByteString -> ByteString -> ActionM ()
    saveUser username hashedPassword = do
      _ <- lift $ runApp connectInfoConfig $ do
        conn <- getUserConn
        lift $ R.runRedis conn $ R.set username hashedPassword
      return ()
  in
    do
      usernameRaw <- param "username"
      password <- param "password"
      let username = (cs . T.toLower $ usernameRaw) :: ByteString
      alreadyExists <- usernameExists username
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
                      saveUser username hashedPassword
                      createSession connectInfoConfig username
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


createSession :: ConnectInfoConfig -> ByteString -> ActionM ()
createSession connectInfoConfig username =
  let
    setSessionCookie :: Text -> ActionM ()
    setSessionCookie token = setCookie cookie
      where
        cookie = (makeSimpleCookie loginCookieName token) {
          setCookieMaxAge = Just (secondsToDiffTime loginTimeout),
          setCookieSecure = True,
          setCookieHttpOnly = True,
          setCookiePath = Just "/",
          setCookieSameSite = Just sameSiteStrict
        }
    saveSessionCookie :: Text -> ActionM ()
    saveSessionCookie token =
      lift $ runApp connectInfoConfig $ saveSession username token
  in
    do
      token <- cs <$> lift GUID.genText
      saveSessionCookie token
      setSessionCookie token
