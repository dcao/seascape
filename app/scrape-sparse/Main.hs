{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}
module Main where

import qualified Data.ByteString as BS
import Data.Csv
import Data.Functor.Identity
import Data.Text
import Data.Text.Read
import GHC.Generics (Generic)
import Streamly
import qualified Streamly.Prelude as S
import Streamly.Csv (encode)
import System.IO
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Text.HTML.Scalpel

import Webdriver

data SectionQtr = SectionQtr
  { instr :: Text
  , course :: Text
  , term :: Text
  , enrolled :: Text
  , evals :: Text
  , recClass :: Text
  , recInstr :: Text
  , hours :: Text
  , gpaExp :: Text
  , gpaAvg :: Text
  } deriving stock (Generic, Show)
    deriving anyclass (ToRecord, FromRecord)

main :: IO ()
main = do
  conf <- firefoxConfig
  src <- runSession conf getSparseResults
  let l = maybe [] id $ scrapeStringLike src sparseHTMLParser
  let s = S.fromList l :: SerialT IO SectionQtr
  let c = Streamly.Csv.encode Nothing s
  withFile "data.csv" WriteMode $ \h ->
    S.mapM_ (BS.hPut h) c

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

unwrap :: Show a => Either a b -> b
unwrap (Left a) = error $ show a
unwrap (Right a) = a

sparseHTMLParser :: Scraper Text [SectionQtr]
sparseHTMLParser = chroots "tr" sectionQtr
  where
    sectionQtr :: Scraper Text SectionQtr
    sectionQtr = do
      (instr:course:term:enrolled:evals:recClass:recInstr:hours:gpaExp:gpaAvg:rs) <- texts "td"
      return $ SectionQtr (strip instr) (parseCourse course) term (strip enrolled) (strip evals) (strip recClass) (strip recInstr) (strip hours) (strip gpaExp) (strip gpaAvg)

    parseCourse :: Text -> Text
    parseCourse = strip . Data.Text.takeWhile (/= '-')
