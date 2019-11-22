{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Seascape.App where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static

import Seascape.Views.Home

type App = SpockM () () () ()
type AppAction a = SpockAction () () () a

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  get root $ lucid $ homeView
