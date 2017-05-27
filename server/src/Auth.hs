module Auth where

import Web.Scotty

import Data.Monoid (mconcat)


app = scottyApp $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
