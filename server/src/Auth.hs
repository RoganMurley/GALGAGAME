module Auth where

import Control.Monad.Trans.Class (lift)
import Data.Monoid (mconcat)
import Data.String.Conversions (cs)
import Data.Text.Internal.Lazy (Text)
import Network.Wai (Application)
import Web.Scotty
import Web.Scotty.Cookie (deleteCookie, setSimpleCookie)

import qualified Data.GUID as GUID
import qualified Database.Redis as R


app :: IO Application
app = do
  conn <- R.connect $ R.defaultConnectInfo { R.connectHost = "redis" }
  scottyApp $ do
    get "/login" $ do
      html $
        "<form action=\"login\" method=POST><input type=\"text\" name=\"username\"><input type=\"password\" name=\"password\"><input type=\"submit\" value=\"Login\"></form><br><form action=\"logout\" method=POST><input type=\"submit\" value=\"Logout\"></form><br><form action=\"register\" method=POST><input type=\"text\" name=\"username\"><input type=\"password\" name=\"password\"><input type=\"submit\" value=\"Register\"></form>"
    post "/login" $ do
      (username :: Text) <- param "username"
      (password :: Text) <- param "password"
      storedPassword <- lift . (R.runRedis conn) $ R.get (cs username)
      case storedPassword of
        Right (Just toad) ->
          case (cs toad) == password of
            True -> do
              token <- lift GUID.genText
              setSimpleCookie "login" token
              html $ mconcat ["<h1>Welcome, ", username, "!</h1>"]
            False ->
              html "Incorrect password."
        _ ->
          html "redis lol"

    post "/logout" $ do
      deleteCookie "login"
      html "Logged out."
    post "/register" $ do
      username <- param "username"
      password <- param "password"
      _ <- lift . (R.runRedis conn) $ R.set username password
      html "Registered."
