{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Seascape.App where

import Control.Monad.Trans
import Data.Text (pack, Text)
import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Pipes.Safe

import Seascape.Data.Sparse
import Seascape.Views.Home

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  get root $ lucid $ homeView
  get "testAgg" $ (text =<< (pack . show) <$> (liftIO $ runSafeT $ aggByQtr (loadRows defaultDataLoc :: SFrame (SafeT IO) SectionQtr)))
