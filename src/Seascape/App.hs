{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications #-}
module Seascape.App where

import qualified Codec.Base64Url as B64
import qualified Control.Foldl as L
import Control.Monad.Trans
import Data.Either (either)
import Data.List (sortBy, findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Frames
import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

import Seascape.Actions.Search
import Seascape.Data.Sparse
import Seascape.Views.Home
import Seascape.Views.Listing
import Seascape.Views.Partials
import Seascape.Views.Section

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

getFrame :: IO (Frame SectionTermIx)
getFrame = loadFrameIx defaultDataLoc

-- TODO: Pagination
-- TODO: For empty queries, show all courses
sectionListingAction :: AggMap -> SectionSearchEng -> Maybe Text -> AppAction ()
sectionListingAction dfm _ Nothing = lucid $ searchView Nothing $ aggMapToFrame dfm
sectionListingAction dfm e (Just q) = do
  let results = execSearch e q
  lucid $ searchView (Just q) $ frameFromICs results dfm

ciToSection :: Either String Text -> Either String Text -> AggMap -> Either String Section
ciToSection c i m = do
  cs <- c
  is <- i
  section <- getsec $ Map.lookup (is, cs) m
  return (is &: cs &: section)

  where
    getsec (Just q) = Right q
    getsec Nothing  = Left "Course/instr not found in database"

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  frame <- liftIO getFrame
  let aggMap = aggByTermMap frame
  let frameAgg = genSectionAgg $ aggMapToFrame aggMap
  let sectionEng = sectionSearchEngine aggMap
  let aggedLen = length aggMap

  get root $ lucid $ homeView aggedLen (return ())

  get "listing" $ do
    query <- param "q"
    sectionListingAction aggMap sectionEng query

  get ("section" <//> (var :: Var Text) <//> (var :: Var Text)) $ \c i -> do
    let ci = decodeUtf8 <$> B64.decode c
    let ins = decodeUtf8 <$> B64.decode i
    let sec = ciToSection ci ins aggMap
    let f b = (do
                let x = filterFrame (\r -> rgetField @Course r == rgetField @Course b) frameAgg
                let rnks = sortBy (\a a' -> compare (rgetField @RecInstrRank a) (rgetField @RecInstrRank a'))
                         $ L.fold L.list x
                let rnk = fromJust
                        $ findIndex (\r -> rgetField @Instr r == rgetField @Instr b)
                        $ rnks
                let df = filterFrame (\r -> (rgetField @Course r == rgetField @Course b) && (rgetField @Instr r == rgetField @Instr b)) frame
                sectionView (rnk + 1) (rgetField @RecInstrRank $ rnks !! rnk) (length rnks) aggedLen df b
              )
    lucid $ either (\a -> homeView aggedLen (errAlert $ pack a)) f sec

  get ("raw" <//> "search") $ do
    query <- param "q"
    text $ pack $ show $ execSearchExplain sectionEng (fromJust query)

  get ("raw" <//> "sections") $ text $ pack $ show frame

  get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show (aggMapToFrame aggMap)

  get ("raw" <//> "sections" <//> "sectionAgg") $ text $ pack $ show frameAgg
