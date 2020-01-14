{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications #-}
module Seascape.App where

import qualified Codec.Base64Url as B64
import Control.Monad.Trans
import Data.Either (either, fromRight)
import qualified Data.Map.Strict as Map
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

type App = SpockM () () () ()
type AppAction = SpockAction () () ()

sectionListingAction :: SectionMap -> SectionSearchEng -> Maybe Text -> AppAction ()
sectionListingAction sm _ Nothing = lucid $ searchView Nothing sm
sectionListingAction sm e (Just q) = do
  let results = execSearch e q
  -- TODO: Putting genSectionAgg here generates ranks based only on the matched
  -- results; we need to have ranks based on everyone
  -- This will prolly necessitate a refactoring of Data.Sparse
  -- Maybe just use AggMap? No need for frames. AggMap should include rank data, etc.
  -- See https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/ for how to
  -- convert csv to a custom type. We should convert each row to a tuple, then turn the
  -- overall list of tuples to a map.
  lucid $ searchView (Just q) $ Map.fromList $ (\x -> (x, fromJust $ Map.lookup x sm)) <$> results

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
    sectionListingAction sectionMap sectionEng query

  get ("section" <//> (var :: Var Text) <//> (var :: Var Text)) $ \c i -> do
    let ins = decodeUtf8 <$> B64.decode i
    let ci = decodeUtf8 <$> B64.decode c
    let sec = ciToSection ins ci sectionMap
    let f (sid, sinfo) = (do
                let x = Map.filterWithKey (\k _ -> course k == course sid) sectionMap
                let ranks = rankBy (\_ info -> recInstr info) x
                let raw = filter (\(Section (k, _)) -> k == sid) sections
                sectionView (fromJust $ Map.lookup sid ranks) (recInstrRank sinfo) (length ranks) (length sectionMap) raw (sid, sinfo)
              )
    lucid $ either (\a -> homeView (length sectionMap) (errAlert $ pack a)) f sec

  -- get ("raw" <//> "search") $ do
  --   query <- param "q"
  --   text $ pack $ show $ execSearchExplain sectionEng (fromJust query)

  -- get ("raw" <//> "sections") $ text $ pack $ show frame

  -- get ("raw" <//> "sections" <//> "noTerm") $ text $ pack $ show (aggMapToFrame aggMap)

  -- get ("raw" <//> "sections" <//> "sectionAgg") $ text $ pack $ show frameAgg
