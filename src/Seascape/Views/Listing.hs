{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Listing where

import qualified Codec.Base64Url as B64
import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Lucid
import Seascape.Data.Sparse
import Seascape.Views.Partials

topHero :: Int -> Maybe Text -> SearchOrdering -> Html ()
topHero ln query od =
  sectionHero $ do
    h1_ [class_ "text-3xl font-medium font-sans"] "Course listing"
    p_ [class_ "text-lg font-serif mt-3 text-teal-600"] $ do
      strong_ $ toHtml $ show ln <> " results"
      toHtml $ maybe " total" (\q -> " found for \"" <> q <> "\"") query
    with (searchBar (maybe "" id query) od) [class_ " mt-8 "]

searchView :: (Ord a, Ord b)
           => Maybe Text
           -> SearchOrdering
           -> Map.Map (a, Text) [(b, Section Int Int)]
           -> Html ()
searchView query od sm = defaultPartial (maybe "Listing - Seascape" (\q -> q <> " - Seascape") query) $ do
  topHero (sum $ length <$> sm) query od
  div_ [class_ "max-w-5xl px-4 mx-auto"] $ do
    forM_ (Map.toList sm) $ \((_, c), rs) -> do
      div_ [class_ "mt-8"] $ do
        p_ [class_ "text-lg mb-3"] $ do
          strong_ $ toHtml c
          " instructors"
        forM_ rs $ \(_, Section (sid, sinfo)) -> do
          div_ [class_ "items-center mb-2 sm:mb-1 border rounded-lg px-5 py-6 sm:p-4 flex flex-col sm:flex-row"] $ do
            div_ [class_ "w-full sm:w-1/3 text-left flex flex-row sm:flex-col items-end sm:items-start"] $ do
              h1_ [class_ "sm:text-lg font-bold sm:mb-1 flex-grow"] $ do
                let cs = B64.encode $ encodeUtf8 $ course sid
                let is = B64.encode $ encodeUtf8 $ instr sid
                a_ [href_ ("/section/" <> cs <> "/" <> is), class_ "text-teal-600 hover:bg-teal-200"] $ toHtml $ unpack $ instr sid
              p_ [class_ "text-sm sm:text-base text-gray-600 text-right sm:text-left"] $ do
                strong_ $ toHtml $ show $ evals sinfo
                " evaluations"
            div_ [class_ "w-full sm:w-2/3 flex flex-row text-left sm:text-right mt-3 sm:mt-0"] $ do
              div_ [class_ "w-1/3 flex flex-col"] $ do
                h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml ("#" <> (show $ recInstrRank sinfo))
                p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "overall rank"
              div_ [class_ "w-1/3 flex flex-col"] $ do
                h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml $ (roundToStr 1 $ recInstr sinfo) <> "%"
                p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "rec. prof."
              div_ [class_ "w-1/3 flex flex-col"] $ do
                h1_ [class_ "font-medium text-sm sm:text-lg font-mono"] $ toHtml $ timeFmt $ hours sinfo
                p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "time/wk"
              div_ [class_ "w-1/3 flex flex-col"] $ do
                with (gpaToHtml $ gpaAvg sinfo) [class_ " text-sm whitespace-no-wrap"]
                p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "avg. GPA"

