module Auth where

import Data.Monoid (mconcat)
import Data.String.Conversions (cs)
import Data.Text.Internal.Lazy (Text)
import Network.Wai (Application)
import Web.Scotty
import Web.Scotty.Cookie (deleteCookie, setSimpleCookie)


app :: IO Application
app = scottyApp $ do
  get "/login" $ do
    html $
      "<form action=\"login\" method=POST><input type=\"text\" name=\"username\"><input type=\"password\" name=\"password\"><input type=\"submit\" value=\"Login\"></form><br><form action=\"logout\" method=POST><input type=\"submit\" value=\"Logout\"></form>"
  post "/login" $ do
    username <- param "username"
    password <- param "password"
    case login username password of
      True -> do
        setSimpleCookie "login" (cs username)
        html $ mconcat ["<h1>Welcome, ", username, "!</h1>"]
      False ->
        html "Incorrect password."
  post "/logout" $ do
    deleteCookie "login"
    html "Logged out."


login :: Text -> Text -> Bool
login username password =
    elem (username, password) users
  where
    users :: [(Text, Text)]
    users = [("master", "sword")]
