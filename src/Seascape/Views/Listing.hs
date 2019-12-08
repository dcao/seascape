{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Seascape.Views.Listing where

import qualified Codec.Base64Url as B64
import qualified Control.Foldl as L
import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, maybe)
import Data.Text (unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Frames
import Lucid
import Seascape.Data.Sparse
import Seascape.Views.Partials
import Text.Printf

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

gpaToLetter :: Double -> String
gpaToLetter x
  | x >= 4.0  = "A"
  | x >= 3.7  = "A-"
  | x >= 3.3  = "B+"
  | x >= 3.0  = "B"
  | x >= 2.7  = "B-"
  | x >= 2.3  = "C+"
  | x >= 2.0  = "C"
  | x >= 1.7  = "C-"
  | x >= 1.0  = "D"
  | otherwise = "F"

gpaToHtml :: Double -> Html ()
gpaToHtml (-1) = h1_ [class_ "font-medium text-lg font-mono text-gray-500"] "N/A"
gpaToHtml x =
  h1_ [class_ "font-medium text-lg font-mono"] $ do
    toHtml $ gpaToLetter x
    span_ [class_ "text-gray-700"] $ toHtml $ " (" <> roundToStr 2 x <> ")"

hrsToHM :: Double -> (Int, Int)
hrsToHM x = floor . (*) 60 <$> properFraction x

topHero :: Int -> Maybe Text -> Html ()
topHero ln query =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-3xl font-medium font-sans"] "Course listing"
      p_ [class_ "text-lg font-serif mt-3 text-teal-600"] $ do
        strong_ $ toHtml $ show ln <> " results"
        toHtml $ maybe " total" (\q -> " found for \"" <> q <> "\"") query
      with (searchBar $ maybe "" id query) [class_ " mt-8 "]

searchView :: Maybe Text -> Frame Section -> Html ()
searchView query df = defaultPartial (maybe "Listing - Seascape" (\q -> unpack q <> " - Seascape") query) $ do
  topHero (frameLen df) query
  div_ [class_ "max-w-5xl px-4 mx-auto"] $ do
    forM_ courses $ \c -> do
      let rs = fromJust $ lookup c dfg
      div_ [class_ "mt-8"] $ do
        p_ [class_ "text-lg mb-3"] $ do
          strong_ $ toHtml c
          " instructors"
        forM_ rs $ \r -> do
          div_ [class_ "items-center mb-1 border rounded-lg px-4 py-4 flex"] $ do
            div_ [class_ "w-2/3 flex flex-col"] $ do
              h1_ [class_ "text-lg font-bold mb-1"] $ do
                let cs = B64.encode $ encodeUtf8 $ rgetField @Course r :: Text
                let is = B64.encode $ encodeUtf8 $ rgetField @Instr r :: Text
                a_ [href_ ("/section/" <> cs <> "/" <> is), class_ "text-teal-600 hover:bg-teal-200"] $ toHtml $ unpack $ rgetField @Instr r
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
              let (h, m) = hrsToHM $ rgetField @Hours r
              h1_ [class_ "font-medium text-lg font-mono"] $ toHtml $ (printf "%d:%02d" h m :: String)
              p_ [class_ "text-sm text-gray-600"] $ "time/wk"
            div_ [class_ "w-1/3 flex flex-col text-right"] $ do
              gpaToHtml $ rgetField @GpaAvg r
              p_ [class_ "text-sm text-gray-600"] $ "avg. GPA"

  where
    -- This list exists because we want to present the courses in order of match,
    -- not in order of the Ord instance of Text (as Map would do if we just forM_'d
    -- over that)
    courses = L.fold L.nub $ L.fold L.list $ fmap (\r -> rgetField @Course r) df
    dfg = Map.toList $ L.fold (L.groupBy (\x -> rgetField @Course x) L.list) df

