{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}
module Main where

import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.Csv
import Data.Text (Text, isInfixOf)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Streamly
import qualified Streamly.Prelude as S
import Streamly.Csv (encode)
import System.IO
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Text.HTML.Scalpel
import Text.HTML.TagSoup.Fast

import BS
import Webdriver

data SectionTerm = SectionTerm
  { instr :: ByteString
  , course :: ByteString
  , term :: ByteString
  , enrolled :: ByteString
  , evals :: ByteString
  , recClass :: ByteString
  , recInstr :: ByteString
  , hours :: ByteString
  , gpaExp :: Maybe ByteString
  , gpaAvg :: Maybe ByteString
  } deriving stock (Generic, Show)
    deriving anyclass (ToRecord, FromRecord)

main :: IO ()
main = do
  conf <- firefoxConfig
  src <- runSession conf getSparseResults
  let tags = parseTags . encodeUtf8 $ src
  let l = maybe [] id $ scrape sparseHTMLParser tags
  let s = S.fromList l :: SerialT IO SectionTerm
  let hdr = V.fromList ["instr", "course", "term", "enrolled", "evals", "recClass", "recInstr", "hours", "gpaExp", "gpaAvg"]
  let c = Streamly.Csv.encode (Just hdr) s
  withFile "data/data.csv" WriteMode $ \h ->
    S.mapM_ (C.hPut h) c

firefoxConfig :: IO WDConfig
firefoxConfig = do
  m <- mgr
  return $ defaultConfig { wdHTTPManager = Just m }

getSparseResults :: WD Text
getSparseResults = do
  -- Set Selenium timeout; not strictly necessary? Idk
  setTimeout 1200000
  openPage "https://cape.ucsd.edu/responses/Results.aspx?Name=whatever"
  waitUntil 60 $ expect =<< (isInfixOf "CAPE Results") <$> getTitle 
  openPage "https://cape.ucsd.edu/responses/Results.aspx?Name=%2C"
  getSource

-- TODO: Use megaparsec-tagsoup instead?
-- TODO: just roll our own megaparsec parsers?
-- TODO: or use tagsoup directly?
--
-- Profiling results:
-- A lot of time is spent in tagsoup functions
-- ergo we need a faster alternative to tagsoup
-- search "haskell tagsoup performance"
-- e.g. fast-tagsoup, html-parse
--
-- The main slow part is tagsoup's parsing step
-- A possible first step is to try to use
-- fast-tagsoup to produce a [Tag ByteString],
-- then pass this into scalpel's `scrape` fn
-- instead of giving it a Text of the HTML
-- source.
sparseHTMLParser :: Scraper ByteString [SectionTerm]
sparseHTMLParser = chroots "tr" sectionTerm
  where
    sectionTerm :: Scraper ByteString SectionTerm
    sectionTerm = do
      (instr:course:term:enrolled:evals:recClass:recInstr:hours:gpaExp:gpaAvg:rs) <- texts "td"
      return $ SectionTerm (bstrip instr) (parseCourse course) term (bstrip enrolled) (bstrip evals) (parsePct recClass) (parsePct recInstr) (parseHours hours) (parseGpa gpaExp) (parseGpa gpaAvg)

    parsePct :: ByteString -> ByteString
    parsePct = dropEnd 2 . bstrip

    parseGpa :: ByteString -> Maybe ByteString
    parseGpa = parseGpaClean . bstrip

    parseGpaClean :: ByteString -> Maybe ByteString
    parseGpaClean "N/A" = Nothing
    parseGpaClean ps    = Just . dropEnd 1 . C.drop 1 . C.dropWhile (/= '(') $ ps

    -- There's literally ONE class with an N/A for hours which requires us to
    -- do this
    parseHours :: ByteString -> ByteString
    parseHours = parseHoursClean . bstrip

    parseHoursClean :: ByteString -> ByteString
    parseHoursClean "N/A" = "0"
    parseHoursClean x     = x

    parseCourse :: ByteString -> ByteString
    parseCourse = bstrip . C.takeWhile (/= '-')
