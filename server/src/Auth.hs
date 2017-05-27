module Auth where

import Data.Monoid (mconcat)
import Data.Text.Internal.Lazy (Text)
import Network.Wai (Application)
import Web.Scotty


app :: IO Application
app = scottyApp $ do
  post "/login" $ do
    username <- param "username"
    password <- param "password"
    case login username password of
      True ->
        html $ mconcat ["<h1>Welcome, ", username, "!</h1>"]
      False ->
        html "Incorrect password."

login :: Text -> Text -> Bool
login username password =
    elem (username, password) users
  where
    users :: [(Text, Text)]
    users = [("master", "sword")]
