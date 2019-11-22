{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}
module Main where

import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.Csv
import Data.Functor.Identity
import Data.Text (Text, isInfixOf)
import Data.Text.Encoding (encodeUtf8)
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

data SectionQtr = SectionQtr
  { instr :: ByteString
  , course :: ByteString
  , term :: ByteString
  , enrolled :: ByteString
  , evals :: ByteString
  , recClass :: ByteString
  , recInstr :: ByteString
  , hours :: ByteString
  , gpaExp :: ByteString
  , gpaAvg :: ByteString
  } deriving stock (Generic, Show)
    deriving anyclass (ToRecord, FromRecord)

main :: IO ()
main = do
  conf <- firefoxConfig
  src <- runSession conf getSparseResults
  let tags = parseTags . encodeUtf8 $ src
  let l = maybe [] id $ scrape sparseHTMLParser tags
  let s = S.fromList l :: SerialT IO SectionQtr
  let c = Streamly.Csv.encode Nothing s
  withFile "data.csv" WriteMode $ \h ->
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
sparseHTMLParser :: Scraper ByteString [SectionQtr]
sparseHTMLParser = chroots "tr" sectionQtr
  where
    sectionQtr :: Scraper ByteString SectionQtr
    sectionQtr = do
      (instr:course:term:enrolled:evals:recClass:recInstr:hours:gpaExp:gpaAvg:rs) <- texts "td"
      return $ SectionQtr (bstrip instr) (parseCourse course) term (bstrip enrolled) (bstrip evals) (bstrip recClass) (bstrip recInstr) (bstrip hours) (bstrip gpaExp) (bstrip gpaAvg)

    parseCourse :: ByteString -> ByteString
    parseCourse = bstrip . C.takeWhile (/= '-')
