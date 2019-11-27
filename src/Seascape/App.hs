{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Seascape.App where

import Control.Monad.Trans
import Data.Text (pack)
import Frames
import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Seascape.Data.Sparse
import Seascape.Views.Home

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

getFrame :: IO (Frame SectionTerm)
getFrame = loadFrame defaultDataLoc

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  frame <- liftIO getFrame
  get root $ lucid $ homeView
  get ("raw" <//> "sections") $ text $ pack $ show frame
  get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show $ aggByTerm frame
