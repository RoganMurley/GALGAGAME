module Auth.Views where

import Config (App, ConnectInfoConfig(..), runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
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
import System.Log.Logger (Priority(DEBUG), debugM, infoM, setLevel, updateGlobalLogger)
import Text.Printf (printf)

import qualified Data.GUID as GUID

import Data.Text (Text)
import qualified Data.Text as T

import Auth.Apps (checkAuth, checkPassword, deleteToken, legalName, legalPassword, loginCookieName, loginTimeout, saveSession, saveUser)


app :: ConnectInfoConfig -> App Application
app config = do
  liftIO $ updateGlobalLogger "auth" $ setLevel DEBUG
  liftIO $ scottyApp $ do
    get  "/auth/me"       $ meView       config
    post "/auth/login"    $ loginView    config
    post "/auth/logout"   $ logoutView   config
    post "/auth/register" $ registerView config


meView :: ConnectInfoConfig -> ActionM ()
meView config = do
  token     <- getCookie loginCookieName
  usernameM <- lift . runApp config $ checkAuth token
  case usernameM of
    Just username -> do
      json $ object [ "username" .= username ]
    Nothing -> do
      json $ object []
  status ok200


loginView :: ConnectInfoConfig -> ActionM ()
loginView config = do
  username <- cs . T.toLower <$> param "username"
  password <- param "password"
  result <- lift $ runApp config $ checkPassword username password
  case result of
    True -> do
      createSession config username
      json $ object []
      status ok200
    False -> do
      lift $ infoM "auth" $ printf "Username not found: %s" (show username)
      json $ object [ "error" .= ("Wrong username or password" :: Text) ]
      status unauthorized401


logoutView :: ConnectInfoConfig -> ActionM ()
logoutView config = do
  token <- getCookie loginCookieName
  case token of
    Just t -> do
      lift $ runApp config $ deleteToken t
      deleteCookie loginCookieName
    Nothing ->
      return ()
  json $ object []
  status ok200


registerView :: ConnectInfoConfig -> ActionM ()
registerView config =
  do
    email          <- param "email"
    usernameRaw    <- param "username"
    password       <- param "password"
    contactableRaw <- param "contactable"
    let username    = cs . T.toLower $ usernameRaw :: ByteString
    let contactable = parseContactable contactableRaw :: Bool
    createUser config email username password contactable
  where
    parseContactable :: ByteString -> Bool
    parseContactable "on" = True
    parseContactable _    = False


createUser :: ConnectInfoConfig -> ByteString -> ByteString -> ByteString  -> Bool -> ActionM ()
createUser config email username password contactable =
  case legalName username *> legalPassword password of
    Just err -> do
      liftIO $ debugM "auth" $ "Creating user " <> cs username <> "error: " <> cs err
      json $ object [ "error" .= err ]
      status badRequest400
    Nothing -> do
      p <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy password
      case p of
        Just hashedPassword -> do
          success <- liftIO $ runApp config $ saveUser email username hashedPassword contactable
          case success of
            True -> do
              createSession config username
              json $ object []
              status created201
            False -> do
              lift $ debugM "auth" ("Registration: user " <> (cs username) <> " already exists")
              json $ object [ "error" .= ("Username already exists" :: Text) ]
              status conflict409
        Nothing -> do
          liftIO $ debugM "auth" "Password hashing error"
          status internalServerError500


createSession :: ConnectInfoConfig -> ByteString -> ActionM ()
createSession config username = do
  token <- cs <$> lift GUID.genText
  lift $ runApp config $ saveSession username token
  let cookie = (makeSimpleCookie loginCookieName token) {
    setCookieMaxAge = Just (secondsToDiffTime loginTimeout),
    setCookieSecure = True,
    setCookieHttpOnly = True,
    setCookiePath = Just "/",
    setCookieSameSite = Just sameSiteStrict
  }
  setCookie cookie
