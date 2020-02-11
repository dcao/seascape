{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications #-}
module Seascape.App where

import qualified Codec.Base64Url as B64
import Control.Monad.Trans
import Data.Either (either, fromRight)
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
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
import Seascape.Views.Plan
import Seascape.Utils

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

sectionListingAction :: SearchOrdering -> SectionMap -> SectionSearchEng -> Maybe Text -> AppAction ()
sectionListingAction so sm e mt = lucid $ searchView mt so cotosec
  where
    results :: Maybe Text -> [(Int, SectionID)]
    results Nothing  = zip [0..] $ Map.keys sm
    results (Just q) = zip [0..] $ execSearch e q

    courses :: Maybe Text -> [Text]
    courses Nothing    = nub $ course <$> Map.keys sm
    courses j@(Just _) = nub $ course . snd <$> (results j)

    cotosec = fmap (fmap snd . sort)
            $ groupBy' fst
            $ fmap (addCourseKey (courses mt) . addSectionOrder sm (ordf so))
            $ results mt

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x) = Right x
maybeToEither y Nothing  = Left y

ciToSection :: Either String Text
            -> Either String Text
            -> SectionMap
            -> Either String (SectionID, SectionInfo Int Int)
ciToSection i c m = do
  cs <- c
  is <- i
  section <- maybeToEither "Course/instr not found in database" $ Map.lookup (SectionID is cs) m
  return ((SectionID is cs), section)

app :: App
app = do
  middleware (staticPolicy (addBase "static"))
  middleware logStdout
  sections <- liftIO $ (fromRight undefined) <$> readSections "data/data.csv"
  let sectionMap = genSectionMap sections
  let sectionEng = sectionSearchEngine sectionMap

  get root $ lucid $ homeView (length sectionMap) (return ())

  get "listing" $ do
    query <- param "q"
    sortBy <- param "sortBy"
    sectionListingAction (getOrdering sortBy) sectionMap sectionEng query

  get ("section" <//> (var :: Var Text) <//> (var :: Var Text)) $ \c i -> do
    let ins = decodeUtf8 <$> B64.decode i
    let ci = decodeUtf8 <$> B64.decode c
    let sec = ciToSection ins ci sectionMap
    let f (sid, sinfo) = (do
                let x = Map.filterWithKey (\k _ -> course k == course sid) sectionMap
                let ranks = rankBy (\_ info -> recInstr info) x
                let raw = filter (\(Section (SectionID _ crs, _)) -> crs == course sid) sections
                sectionView (fromJust $ Map.lookup sid ranks) (recInstrRank sinfo) (length ranks) (length sectionMap) raw (sid, sinfo)
              )
    lucid $ either (\a -> homeView (length sectionMap) (errAlert $ pack a)) f sec

  get "plan" $ do
    lucid planView

  -- get ("raw" <//> "search") $ do
  --   query <- param "q"
  --   text $ pack $ show $ execSearchExplain sectionEng (fromJust query)

  -- get ("raw" <//> "sections") $ text $ pack $ show frame

  -- get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show (aggMapToFrame aggMap)

  -- get ("raw" <//> "sections" <//> "sectionAgg") $ text $ pack $ show frameAgg
