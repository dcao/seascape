{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Seascape.App where

import Control.Monad.Trans
import Data.Maybe (fromJust)
import Data.Text (pack)
import Frames
import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Seascape.Actions.Search
import Seascape.Data.Sparse
import Seascape.Views.Home
import Seascape.Views.Listing

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

getFrame :: IO (Frame SectionTerm)
getFrame = loadFrame defaultDataLoc

-- TODO: Pagination
-- TODO: For empty queries, show all courses
sectionListingAction :: AggMap -> SectionSearchEng -> Text -> AppAction ()
sectionListingAction dfm e q = do
  let results = execSearch e q
  lucid $ searchView q $ frameFromICs results dfm

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  frame <- liftIO getFrame
  let aggMap = aggByTermMap frame
  let sectionEng = sectionSearchEngine aggMap
  let aggedLen = length aggMap

  get root $ lucid $ homeView aggedLen

  get "listing" $ do
    query <- param "q"
    maybe (redirect "/") (sectionListingAction aggMap sectionEng) query

  get ("raw" <//> "search") $ do
    query <- param "q"
    text $ pack $ show $ execSearchExplain sectionEng (fromJust query)

  get ("raw" <//> "sections") $ text $ pack $ show frame

  get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show (aggMapToFrame aggMap)
