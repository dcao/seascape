{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Seascape.Views.Section where

import Data.Foldable (forM_)
import Data.Text (pack)
import Frames
import Lucid
import Seascape.Data.Sparse
import Seascape.Views.Partials

topHero :: Int -> Int -> Int -> Int -> Section -> Html ()
topHero rnki rnko cnti cnto s =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [id_ "summary", class_ "mb-6"] $ do
      div_ [class_ "flex flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "flex flex-col flex-grow text-left"] $ do
          h1_ [class_ "text-3xl font-semibold font-sans"] $ toHtml $ rgetField @Instr s
          h1_ [class_ "text-2xl font-medium font-sans"] $ toHtml $ rgetField @Course s
        div_ [class_ "flex flex-col text-right mr-2 p-4 border rounded border-gray-800"] $ do
          h1_ [class_ "text-2xl font-bold font-sans"] $ toHtml $ "#" <> show rnki <> " of " <> show cnti
          p_ [class_ "text-sm text-gray-600"] $ toHtml $ rgetField @Course s <> " prof. ranking"
        div_ [class_ "flex flex-col text-right p-4 border rounded border-gray-800"] $ do
          h1_ [class_ "text-2xl font-bold font-sans"] $ toHtml $ "#" <> show rnko <> " of " <> show cnto
          p_ [class_ "text-sm text-gray-600"] "Overall section ranking"
    div_ [id_ "report-card"] $ do
      div_ [class_ "flex max-w-5xl mx-auto"] $ do
        p_ [class_ "text-sm tracking-widest uppercase px-2 py-1 bg-teal-300 mr-auto mb-4 rounded"] "Report card"
      div_ [class_ "flex flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "mr-3 w-1/3"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Instructor approval"
          h1_ [class_ "text-xl font-semibold font-mono flex-grow mb-3"] $ do
            toHtml $ (roundToStr 1 $ rgetField @RecInstr s) <> "%"
          div_ [class_ "rec-instr-spark"] $ return ()
        div_ [class_ "mx-3 w-1/3"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Hours committed"
          h1_ [class_ "text-xl font-semibold font-mono mb-3"] $ do
            toHtml $ (timeFmt $ rgetField @Hours s) <> " / wk"
          div_ [class_ "time-spark"] $ return ()
        div_ [class_ "ml-3 w-1/3"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Average grade"
          div_ [class_ "flex flex-row"] $ do
            with (gpaToHtml $ rgetField @GpaAvg s) [class_ " font-semibold mb-3 "]
          div_ [class_ "gpa-spark"] $ return ()

midNav :: Html ()
midNav = do
  div_ [class_ "bg-gray-800 py-3 px-2 sticky mb-6"] $ do
    div_ [class_ "flex flex-row items-center justify-center text-center text-gray-200 mx-auto"] $ do
      link "#raw-data" "Raw Data"
      p_ [class_ "text-sm px-2 mx-1 py-1 font-medium text-gray-500 bg-gray-700 rounded"] "More coming soon"

  where
    link href x = with (a_ x) [class_ "px-2 mx-1 py-1 font-medium hover:bg-gray-600 rounded", href_ href]

rawData :: Frame SectionTermIx -> Html ()
rawData df = do
  div_ [id_ "raw-data", class_ "flex flex-col max-w-5xl mx-auto"] $ do
    span_ [class_ "text-sm tracking-widest uppercase px-2 py-1 bg-gray-300 mr-auto mb-4 rounded mb-3"] "Raw data"
    forM_ df $ \r -> do
      div_ [class_ "items-center mb-1 border rounded-lg px-4 py-4 flex"] $ do
        div_ [class_ "w-2/3 flex flex-col"] $ do
          h1_ [class_ "text-lg font-bold mb-1"] $ toHtml $ rgetField @Seascape.Data.Sparse.Term r
          p_ [class_ "text-gray-600"] $ do
            strong_ $ toHtml $ show $ rgetField @Evals r
            " evaluations"
        div_ [class_ "w-1/3 flex flex-col text-right"] $ do
          h1_ [class_ "font-medium text-lg font-mono"] $ toHtml $ (roundToStr 1 $ rgetField @RecClass r) <> "%"
          p_ [class_ "text-sm text-gray-600 text-right"] $ "rec. class"
        div_ [class_ "w-1/3 flex flex-col text-right"] $ do
          h1_ [class_ "font-medium text-lg font-mono"] $ toHtml $ (roundToStr 1 $ rgetField @RecInstr r) <> "%"
          p_ [class_ "text-sm text-gray-600"] $ "rec. instructor"
        div_ [class_ "w-1/3 flex flex-col text-right"] $ do
          h1_ [class_ "font-medium text-lg font-mono"] $ toHtml $ timeFmt $ rgetField @Hours r
          p_ [class_ "text-sm text-gray-600"] $ "time/wk"
        div_ [class_ "w-1/3 flex flex-col text-right"] $ do
          gpaToHtml $ rgetField @GpaAvg r
          p_ [class_ "text-sm text-gray-600"] $ "avg. GPA"

sectionView :: Int -> Int -> Int -> Int -> Frame SectionTermIx -> Section -> Html ()
sectionView rnki rnko cnti cnto df s = defaultPartial (rgetField @Instr s <> " - " <> rgetField @Course s <> " - Seascape") $ do
  topHero rnki rnko cnti cnto s
  midNav
  rawData df
  js_ "/js/d3.v5.min.js"
  js_ "/js/section.js"
  script_ $
    "const sectionTerms = " <> jsonifyFrame df <> ";" <>
    "sparkline('.rec-instr-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_recInstr }; }), " <> (pack $ show $ rgetField @RecInstr s) <> ");" <>
    "sparkline('.time-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_hours }; }), " <> (pack $ show $ rgetField @Hours s) <> ");" <>
    if rgetField @GpaAvg s /= -1 then "sparkline('.gpa-spark', sectionTerms.map(function (r) { return { x: r.st_termIx, y: r.st_gpaAvg }; }), " <> (pack $ show $ rgetField @GpaAvg s) <> ");" else ""
