{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Seascape.Views.Listing where

import qualified Control.Foldl as L
import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (unpack, Text)
import Frames
import Lucid
import Seascape.Data.Sparse
import Seascape.Views.Partials
import Text.Printf

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

gpaToHtml :: Double -> Html ()
gpaToHtml (-1) = h1_ [class_ "font-semibold text-lg text-gray-700"] "N/A"
gpaToHtml x    = h1_ [class_ "font-semibold text-lg"] $ toHtml $ roundToStr 2 x

topHero :: Int -> Text -> Html ()
topHero ln query =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-3xl font-medium font-sans"] "Course listing"
      p_ [class_ "text-lg font-serif mt-3 text-teal-600"] $ do
        strong_ $ toHtml $ show ln <> " results"
        " found for this search."
      with (searchBar query) [class_ " mt-8 "]

searchView :: Text -> Frame Section -> Html ()
searchView query df = defaultPartial (unpack query <> " - Seascape") $ do
  topHero (frameLen df) query
  -- Table of results
  div_ [class_ "max-w-5xl px-4 mx-auto"] $ do
    forM_ dfg $ \(c, rs) -> do
      div_ [class_ "mt-8"] $ do
        p_ [class_ "text-lg mb-3"] $ do
          strong_ $ toHtml c
          " instructors"
        forM_ rs $ \r -> do
          div_ [class_ "items-center mb-1 border rounded-lg px-4 py-4 flex"] $ do
            div_ [class_ "w-1/2 flex flex-col"] $ do
              h1_ [class_ "text-lg font-bold mb-1"] $ toHtml $ unpack $ rgetField @Instr r
              p_ [class_ "text-gray-600"] $ do
                strong_ $ toHtml $ show $ rgetField @Evals r
                " evaluations"
            div_ [class_ "w-1/3 flex flex-col text-right"] $ do
              h1_ [class_ "font-semibold text-lg"] $ toHtml $ (roundToStr 1 $ rgetField @RecClass r) <> "%"
              p_ [class_ "text-sm text-gray-600 text-right"] $ "rec. class"
            div_ [class_ "w-1/3 flex flex-col text-right"] $ do
              h1_ [class_ "font-semibold text-lg"] $ toHtml $ (roundToStr 1 $ rgetField @RecInstr r) <> "%"
              p_ [class_ "text-sm text-gray-600"] $ "rec. instructor"
            div_ [class_ "w-1/3 flex flex-col text-right"] $ do
              h1_ [class_ "font-semibold text-lg"] $ toHtml $ (roundToStr 2 $ rgetField @Hours r) <> " hrs"
              p_ [class_ "text-sm text-gray-600"] $ "time/wk"
            div_ [class_ "w-1/3 flex flex-col text-right"] $ do
              h1_ [class_ "font-semibold text-lg"] $ toHtml $ roundToStr 2 $ rgetField @GpaAvg r
              p_ [class_ "text-sm text-gray-600"] $ "avg. GPA"

  where
    dfg = Map.toList $ L.fold (L.groupBy (\r -> rgetField @Course r) L.list) df
