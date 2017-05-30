module Auth where

import Control.Monad.Trans.Class (lift)
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Monoid (mconcat)
import Data.String.Conversions (cs)
import Network.Wai (Application)
import Web.Scotty
import Web.Scotty.Cookie (deleteCookie, getCookie, setSimpleCookie)

import qualified Data.GUID as GUID
import qualified Database.Redis as R

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T


app :: R.Connection -> R.Connection -> IO Application
app userConn tokenConn =
  scottyApp $ do

    get "/login" $ do
      html $
        "<form action=\"login\" method=POST><input type=\"text\" name=\"username\"><input type=\"password\" name=\"password\"><input type=\"submit\" value=\"Login\"></form><br><form action=\"logout\" method=POST><input type=\"submit\" value=\"Logout\"></form><br><form action=\"register\" method=POST><input type=\"text\" name=\"username\"><input type=\"password\" name=\"password\"><input type=\"submit\" value=\"Register\"></form>"

    post "/login" $ do
      username <- param "username"
      password <- param "password"
      gotten <- lift . (R.runRedis userConn) $ R.get username
      case gotten of
        Right pass ->
          case pass of
            Just hashedPassword ->
              if validatePassword hashedPassword password
                then ( do
                  token <- lift GUID.genText
                  setSimpleCookie "login" token
                  _ <- lift . (R.runRedis tokenConn) $ do
                    _ <- R.set (cs token) username
                    R.expire (cs token) 60
                  html $ mconcat ["<h1>Welcome, ", cs username, "!</h1>"]
                )
                else (html "Incorrect username or password.")
            Nothing ->
              html "Incorrect username or password."
        Left _ -> do
          lift . T.putStrLn $ "Database connection error"
          html "Database connection error"

    post "/logout" $ do
      token <- getCookie "login"
      case token of
        Just t -> do
          _ <- lift . (R.runRedis tokenConn) $ R.del [T.encodeUtf8 t]
          deleteCookie "login"
          html "Logged out."
        Nothing ->
          html "Can't log out; not logged in"

    post "/register" $ do
      username <- param "username"
      password <- param "password"
      p <- lift $ hashPasswordUsingPolicy slowerBcryptHashingPolicy password
      case p of
        Just hashedPassword -> do
          _ <- lift . (R.runRedis userConn) $ R.set username hashedPassword
          html "Registered."
        Nothing ->
          html "Something went wrong with your registration."


checkAuth :: R.Connection -> Token -> IO (Maybe Username)
checkAuth tokenConn token = do
  gotten <- (R.runRedis tokenConn) $ R.get (T.encodeUtf8 token)
  case gotten of
    Right username ->
      return (T.decodeUtf8 <$> username)
    Left _ -> do
      T.putStrLn $ "Database connection error"
      return Nothing


type Token = Text
type Username = Text


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
