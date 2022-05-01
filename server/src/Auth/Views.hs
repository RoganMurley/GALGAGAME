module Auth.Views where

import Auth.Apps (checkAuth, checkPassword, cidCookieName, deleteToken, legalName, legalPassword, saveSession, saveUser, sessionCookieName)
import Config (App, ConnectInfoConfig (..), runApp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import qualified Data.GUID as GUID
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Feedback.Views (feedbackView)
import Leaderboard.Views (leaderboardView)
import League.Views (leagueCheckView, leagueView)
import Network.HTTP.Types.Status
import Network.Wai (Application)
import System.Log.Logger (Priority (DEBUG), debugM, infoM, setLevel, updateGlobalLogger)
import Text.Printf (printf)
import Web.Cookie (sameSiteStrict, setCookieExpires, setCookieHttpOnly, setCookieMaxAge, setCookiePath, setCookieSameSite, setCookieSecure)
import Web.Scotty
import Web.Scotty.Cookie (getCookie, makeSimpleCookie, setCookie)

app :: ConnectInfoConfig -> App Application
app config = do
  liftIO $ updateGlobalLogger "scotty" $ setLevel DEBUG
  liftIO $
    scottyApp $ do
      get "/api/me" $ meView config
      post "/api/login" $ loginView config
      post "/api/logout" $ logoutView config
      post "/api/register" $ registerView config
      post "/api/feedback" $ feedbackView config
      post "/api/league" $ leagueView config
      get "/api/league" $ leagueCheckView config
      get "/api/leaderboard" $ leaderboardView config

meView :: ConnectInfoConfig -> ActionM ()
meView config = do
  token <- getCookie sessionCookieName
  usernameM <- lift . runApp config $ checkAuth token
  case usernameM of
    Just username -> do
      json $ object ["username" .= username]
    Nothing -> do
      json $ object []
  status ok200

loginView :: ConnectInfoConfig -> ActionM ()
loginView config = do
  username <- cs . T.toLower <$> param "username"
  password <- param "password"
  result <- lift $ runApp config $ checkPassword username password
  if result
    then
      ( do
          createSession config username
          json $ object []
          status ok200
      )
    else
      ( do
          lift $ infoM "auth" $ printf "Username not found: %s" (show username)
          json $ object ["error" .= ("Wrong username or password" :: Text)]
          status unauthorized401
      )

logoutView :: ConnectInfoConfig -> ActionM ()
logoutView config = do
  token <- getCookie sessionCookieName
  lift $ infoM "auth" $ printf "Logging out"
  case token of
    Just t ->
      lift $ runApp config $ deleteToken t
    Nothing ->
      return ()
  deleteCookie sessionCookieName
  deleteCookie cidCookieName
  deleteCookie "user"
  json $ object []
  status ok200

registerView :: ConnectInfoConfig -> ActionM ()
registerView config =
  do
    email <- param "email"
    usernameRaw <- param "username"
    password <- param "password"
    contactableRaw <- param "contactable"
    let username = cs . T.toLower $ usernameRaw :: ByteString
    let contactable = parseContactable contactableRaw :: Bool
    createUser config email username password contactable
  where
    parseContactable :: ByteString -> Bool
    parseContactable "on" = True
    parseContactable _ = False

createUser :: ConnectInfoConfig -> ByteString -> ByteString -> ByteString -> Bool -> ActionM ()
createUser config email username password contactable =
  case legalName username *> legalPassword password of
    Just err -> do
      liftIO $ debugM "auth" $ "Creating user " <> cs username <> "error: " <> cs err
      json $ object ["error" .= err]
      status badRequest400
    Nothing -> do
      cid <- getCookie cidCookieName
      p <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy password
      case p of
        Just hashedPassword -> do
          success <- liftIO $ runApp config $ saveUser email username hashedPassword contactable cid
          if success
            then
              ( do
                  createSession config username
                  json $ object []
                  status created201
              )
            else
              ( do
                  lift $ debugM "auth" ("Registration: user " <> cs username <> " already exists")
                  json $ object ["error" .= ("Username already exists" :: Text)]
                  status conflict409
              )
        Nothing -> do
          liftIO $ debugM "auth" "Password hashing error"
          status internalServerError500

createSession :: ConnectInfoConfig -> ByteString -> ActionM ()
createSession config username = do
  token <- cs <$> lift GUID.genText
  lift $ runApp config $ saveSession username token
  let maxAge = secondsToDiffTime (60 * 60 * 24 * 356 * 10)
  let sessionCookie =
        (makeSimpleCookie sessionCookieName token)
          { setCookieMaxAge = Just maxAge,
            setCookieSecure = True,
            setCookieHttpOnly = True,
            setCookiePath = Just "/",
            setCookieSameSite = Just sameSiteStrict
          }
  let userCookie =
        (makeSimpleCookie "user" (cs username))
          { setCookieMaxAge = Just maxAge,
            setCookieSecure = True,
            setCookiePath = Just "/",
            setCookieSameSite = Just sameSiteStrict
          }
  setCookie sessionCookie
  setCookie userCookie

-- Firefox doesn't delete cookies with different parameters, so the default Scotty implementation of deleteCookie won't work.
deleteCookie :: Text -> ActionM ()
deleteCookie name =
  setCookie $
    (makeSimpleCookie name "")
      { setCookieExpires = Just $ posixSecondsToUTCTime 0,
        setCookieSecure = True,
        setCookiePath = Just "/",
        setCookieSameSite = Just sameSiteStrict
      }
