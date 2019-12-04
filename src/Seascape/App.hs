{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Seascape.App where

import Control.Monad.Trans
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
sectionListingAction :: Frame Section -> SectionSearchEng -> Text -> AppAction ()
sectionListingAction df e q = do
  let results = execSearch e q
  lucid $ searchView q $ filterByICs results df

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  frame <- liftIO getFrame
  let agged = aggByTerm frame
  let sectionEng = sectionSearchEngine agged
  let aggedLen = frameLen agged

  get root $ lucid $ homeView aggedLen

  get "listing" $ do
    query <- param "q"
    maybe (redirect "/") (sectionListingAction agged sectionEng) query

  get ("raw" <//> "sections") $ text $ pack $ show frame

  get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show agged
