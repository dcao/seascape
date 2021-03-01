{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Seascape.Views.Section where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (encode)
import Data.Foldable (forM_)
import Data.List (filter)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Text.Encoding
import Lucid hiding (term)
import Seascape.Data.Sparse
import Seascape.Data.Prereqs
import Seascape.Views.Partials
import Seascape.Utils

topHero :: Int -> Int -> Int -> Int -> (SectionID, SectionInfo Int Int) -> Html ()
topHero rnki rnko cnti cnto (sid, sinfo) =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [id_ "summary", class_ "mb-8 sm:mb-6"] $ do
      div_ [class_ "flex flex-col sm:flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "flex flex-col flex-grow text-left"] $ do
          h1_ [class_ "text-2xl sm:text-3xl font-semibold font-sans flex-grow sm:flex-grow-0"] $ toHtml $ instr sid
          h1_ [class_ "text-xl sm:text-2xl font-medium font-sans"] $ toHtml $ course sid
        div_ [class_ "flex flex-row mt-3 sm:mt-0"] $ do
          div_ [class_ "flex flex-col sm:text-right mr-2 py-3 px-4 sm:py-4 border flex-grow rounded border-gray-800"] $ do
            h1_ [class_ "text-lg sm:text-xl font-semibold sm:font-bold font-sans"] $ toHtml $ "#" <> show rnki <> " of " <> show cnti
            p_ [class_ "text-sm text-gray-600"] $ toHtml $ course sid <> " prof. ranking"
          div_ [class_ "flex flex-col sm:text-right py-3 px-4 sm:py-4 border flex-grow rounded border-gray-800"] $ do
            h1_ [class_ "text-lg sm:text-xl font-semibold sm:font-bold font-sans"] $ toHtml $ "#" <> show rnko <> " of " <> show cnto
            p_ [class_ "text-sm text-gray-600"] "Overall section ranking"
    div_ [id_ "report-card"] $ do
      div_ [class_ "flex max-w-5xl mx-auto"] $ do
        p_ [class_ "text-sm tracking-widest uppercase px-2 py-1 bg-teal-300 mr-auto mb-4 rounded"] "Report card"
      div_ [class_ "flex flex-col sm:flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "sm:mr-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Instructor approval"
          h1_ [class_ "text-xl font-semibold font-mono flex-grow mb-2"] $ do
            toHtml $ (roundToStr 1 $ recInstr sinfo) <> "%"
          div_ [class_ "rec-instr-spark"] $ return ()
        div_ [class_ "sm:mx-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Hours committed"
          h1_ [class_ "text-xl font-semibold font-mono mb-2"] $ do
            toHtml $ (timeFmt $ hours sinfo) <> " / wk"
          div_ [class_ "time-spark"] $ return ()
        div_ [class_ "sm:ml-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Average grade"
          div_ [class_ "flex flex-row"] $ do
            with (gpaToHtml $ gpaAvg sinfo) [class_ " font-semibold mb-2 text-xl "]
          div_ [class_ "gpa-spark"] $ return ()

midNav :: Html ()
midNav = do
  div_ [class_ "bg-gray-800 py-3 px-2 lg:sticky top-0"] $ do
    div_ [class_ "flex flex-col sm:flex-row items-center justify-center text-center text-gray-200 mx-auto"] $ do
      link "#" "Overview"
      link "#difficulty" "Class Difficulty"
      link "#prereqs" "Prerequisites"
      link "#raw-data" "Raw Data"

  where
    link href x = with (a_ x) [class_ "px-2 my-1 py-1 font-medium hover:bg-gray-600 rounded", href_ href]

body :: [Section Int ()] -> [[CourseInfo]] -> SectionID -> Html ()
body df ps sid =
  div_ [class_ "px-6 mt-6"] $ do
    classDiff df sid
    prereqSec ps
    rawData $ filterDf df sid

classDiff :: [Section Int ()] -> SectionID -> Html ()
classDiff df sid =
  sectionSec "difficulty" "Class Difficulty" $ do
    p_ [class_ "text-xl tracking-tight"] $ do
      "This professor takes "
      span_ [class_ "font-semibold border-gray-800 border-bottom border-dotted border-b"] $ toHtml $ hoursStr hourRank
      " and is "
      span_ [class_ "font-semibold border-gray-800 border-bottom border-dotted border-b"] $ toHtml $ gpaStr gpaRank
      " compared to the average "
      span_ [class_ "border-gray-800 border-bottom border-dotted border-b"] $ toHtml $ course sid
      " professor."
    div_ [class_ "flex flex-col sm:flex-row mt-5"] $ do
      div_ [class_ "sm:mr-3 mb-6 w-full sm:w-1/2"] $ do
        p_ [class_ "text-gray-600 text-sm mb-1"] "Time commitment"
        h1_ [class_ "text-lg font-medium flex-grow mb-2"] $ toHtml $ show'
          " smallest time commitment of "
          (length rankedHours)
          (hourRank)
        div_ [class_ "hours-bnw"] mempty
      div_ [class_ "sm:ml-3 mb-6 w-full sm:w-1/2"] $ do
        p_ [class_ "text-gray-600 text-sm mb-1"] "Average grade"
        h1_ [class_ "text-lg font-medium flex-grow mb-2"] $ toHtml $ show'
          " highest grade of "
          (length rankedGpa)
          (gpaRank)
        div_ [class_ "gpa-bnw"] mempty

  where
    grouped = groupBy' (\(Section (tid, _)) -> tid) df
    getMedians xs = ( median $ fmap (\(Section (_, x)) -> hours x) xs
                    , median $ fmap fromGpa $ filter gpaExists $ fmap (\(Section (_, x)) -> gpaAvg x) xs
                    )
    grouped' = fmap getMedians grouped

    rankedHours = rankBy (\_ v -> negate <$> fst v) grouped'
    rankedGpa = rankBy (\_ v -> snd v) grouped'

    show' str l (Just x) = "#" <> show x <> str <> show l
    show' _ _ Nothing = "N/A"

    hourRank = Map.lookup sid rankedHours
    gpaRank = Map.lookup sid rankedGpa

    hoursStr :: Maybe Int -> String
    hoursStr (Just x)
      | length rankedHours <= 2 = "an average amount of time"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.8 = "way more time"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.6 = "more time"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.4 = "an average amount of time"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.2 = "less time"
      | otherwise = "a lot less time"
    hoursStr Nothing = "an unknown amount of time"

    gpaStr :: Maybe Int -> String
    gpaStr (Just x)
      | length rankedHours <= 2 = "of average difficulty"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.8 = "way harder"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.6 = "harder"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.4 = "of average difficulty"
      | (fromIntegral x) / (fromIntegral $ length rankedHours) >= 0.2 = "easier"
      | otherwise = "way easier"
    gpaStr Nothing = "of unknown difficulty"

prereqSec :: [[CourseInfo]] -> Html ()
prereqSec ps =
  sectionSec "prereqs" "Prerequisites (Beta!)" $ do
    p_ [class_ "tracking-tight text-lg mb-4"] "Each of the below requirements must be satisfied:"
    forM_ ps $ \ors -> do
      div_ [class_ "items-center mb-2 sm:mb-1 border rounded-lg px-5 py-6 sm:p-4"] $ do
        p_ [class_ "text-sm text-gray-600 mb-2"] "One of:"
        div_ [class_ "lg:grid lg:grid-cols-3"] $ do
          forM_ ors $ \cs -> do
            div_ [class_ "mb-2"] $ do
              h1_ [class_ "sm:text-lg font-bold sm:mb-1 flex-grow"] $ toHtml $ course_i cs
              p_ [class_ "text-sm text-gray-600"] $ toHtml $ desc cs

rawData :: [Section Int ()] -> Html ()
rawData df =
  sectionSec "raw-data" "Raw Data" $ do
    forM_ df $ \r -> do
      let (_, rinfo) = unSection r
      div_ [class_ "items-center mb-2 sm:mb-1 border rounded-lg px-5 py-6 sm:p-4 flex flex-col sm:flex-row"] $ do
        div_ [class_ "w-full sm:w-1/3 text-left flex flex-row sm:flex-col items-end sm:items-start"] $ do
          h1_ [class_ "sm:text-lg font-bold sm:mb-1 flex-grow"] $ toHtml $ term rinfo
          p_ [class_ "text-sm sm:text-base text-gray-600 text-right sm:text-left"] $ do
            strong_ $ toHtml $ show $ evals rinfo
            " evaluations"
        div_ [class_ "w-full sm:w-2/3 flex flex-row text-left sm:text-right mt-3 sm:mt-0"] $ do
          div_ [class_ "w-1/3 flex flex-col"] $ do
            h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml $ (roundToStr 1 $ recClass rinfo) <> "%"
            p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "rec. class"
          div_ [class_ "w-1/3 flex flex-col"] $ do
            h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml $ (roundToStr 1 $ recInstr rinfo) <> "%"
            p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "rec. prof."
          div_ [class_ "w-1/3 flex flex-col"] $ do
            h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml $ timeFmt $ hours rinfo
            p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "time/wk"
          div_ [class_ "w-1/3 flex flex-col"] $ do
            with (gpaToHtml $ gpaAvg rinfo) [class_ " text-sm whitespace-no-wrap"]
            p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "avg. GPA"

sectionView :: Int -> Int -> Int -> Int -> [Section Int ()] -> [[CourseInfo]] -> (SectionID, SectionInfo Int Int) -> Html ()
sectionView rnki rnko cnti cnto df ps (sid, sinfo) = defaultPartial (instr sid <> " - " <> course sid <> " - Seascape") $ do
  topHero rnki rnko cnti cnto (sid, sinfo)
  midNav
  body df ps sid
  js_ "/js/d3.v5.min.js"
  js_ "/js/section.js"
  script_ $
    "const allTerms = " <> (decodeUtf8 $ LBS.toStrict $ encode df) <> ";" <>
    "const sectionTerms = " <> (decodeUtf8 $ LBS.toStrict $ encode $ filterDf df sid) <> ";"
  script_ $
    "sparkline('.rec-instr-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_recInstr }; }), " <> (pack $ show $ recInstr sinfo) <> ");" <>
    "sparkline('.time-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_hours }; }), " <> (pack $ show $ hours sinfo) <> ");" <>
    (if gpaExists (gpaAvg sinfo) then "sparkline('.gpa-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_gpaAvg }; }), " <> (pack $ show $ lossyGpa $ gpaAvg sinfo) <> ");" else "") <>
    "boxnwhisk('.hours-bnw', allTerms.map(function(r) { return { x: r.st_hours, y: r.st_instr }; }), '" <> instr sid <> "', false, 'hours spent per week');" <>
    "boxnwhisk('.gpa-bnw', allTerms.map(function(r) { return { x: r.st_gpaAvg, y: r.st_instr }; }), '" <> instr sid <> "', true, 'average GPA');"

filterDf :: [Section a b] -> SectionID -> [Section a b]
filterDf df sid = filter (\(Section (sid', _)) -> sid == sid') df
