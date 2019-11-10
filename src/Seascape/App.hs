{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Seascape.App where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static

import Seascape.Model
import Seascape.Views.Partials

import Lucid

type App = SpockM () () () ()
type AppAction a = SpockAction () () () a

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  get root $ lucid $ defaultPartial "Seascape" $ p_ "Hello world!"
