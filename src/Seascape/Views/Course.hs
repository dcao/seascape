{-# LANGUAGE OverloadedStrings, TypeApplications #-}
-- View for just a single course.
module Seascape.Views.Course where

import qualified Codec.Base64Url as B64
import Lucid
import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Seascape.Data.Sparse
import Seascape.Data.Prereqs
import Seascape.Views.Partials

data CourseViewInfo = CourseViewInfo
  { courseName :: Text
  , instrMap   :: SectionMap
  , prereqList :: [[CourseInfo]]
  } deriving (Show)

classAvg :: SectionMap -> Double
classAvg sm = recPct / fromIntegral totEvals
  where
    sml = Map.toList sm

    totEvals = sum $ fmap (evals . snd) sml
    recPct = sum $ fmap (\x -> recClass (snd x) * fromIntegral (evals (snd x))) sml

hoursAvg :: SectionMap -> Double
hoursAvg sm = hoursPct / fromIntegral totEvals
  where
    sml = Map.toList sm

    totEvals = sum $ fmap (evals . snd) sml
    hoursPct = sum $ fmap (\x -> hours (snd x) * fromIntegral (evals (snd x))) sml

gpaCAvg :: SectionMap -> Double
gpaCAvg sm = gpaOv / fromIntegral totEvals
  where
    sml = Map.toList sm

    totEvals = sum $ fmap (evals . snd) sml
    gpaOv = sum $ fmap (\x -> (lossyGpa . gpaAvg) (snd x) * fromIntegral (evals (snd x))) sml

topHero :: CourseViewInfo -> Html ()
topHero cvi =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [id_ "summary", class_ "mb-8 sm:mb-6"] $ do
      div_ [class_ "flex flex-col sm:flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "flex flex-col flex-grow text-left"] $ do
          h1_ [class_ "text-2xl sm:text-3xl font-semibold font-sans flex-grow sm:flex-grow-0"] $ toHtml $ courseName cvi
          h1_ [class_ "text-lg sm:text-xl text-gray-600 font-medium font-sans"] $ toHtml $ show (length $ instrMap cvi) <> " instructors"
    div_ [id_ "report-card"] $ do
      div_ [class_ "flex max-w-5xl mx-auto"] $ do
        p_ [class_ "text-sm tracking-widest uppercase px-2 py-1 bg-teal-300 mr-auto mb-4 rounded"] "Report card"
      div_ [class_ "flex flex-col sm:flex-row max-w-5xl mx-auto"] $ do
        div_ [class_ "sm:mr-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Avg. class approval"
          h1_ [class_ "text-xl font-semibold font-mono flex-grow mb-2"] $ toHtml $ (roundToStr 1 $ classAvg $ instrMap cvi) <> "%"
          div_ [class_ "rec-instr-spark"] $ return ()
        div_ [class_ "sm:mx-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Avg. hours committed"
          h1_ [class_ "text-xl font-semibold font-mono mb-2"] $ do
            toHtml $ timeFmt (hoursAvg $ instrMap cvi) <> " / wk"
          div_ [class_ "time-spark"] $ return ()
        div_ [class_ "sm:ml-3 mb-6 w-full sm:w-1/3 flex-grow"] $ do
          p_ [class_ "text-gray-600 text-sm mb-1"] "Average grade"
          div_ [class_ "flex flex-row"] $ do
            with (gpaNumToHtml $ gpaCAvg $ instrMap cvi) [class_ " font-semibold mb-2 text-xl "]
          div_ [class_ "gpa-spark"] $ return ()

midNav :: Html ()
midNav = do
  div_ [class_ "bg-gray-800 py-3 px-2 lg:sticky top-0"] $ do
    div_ [class_ "flex flex-col sm:flex-row items-center justify-center text-center text-gray-200 mx-auto"] $ do
      link "#" "Overview"
      link "#prereqs" "Prerequisites"
      link "#instrs" "Instructors"

  where
    link href x = with (a_ x) [class_ "px-2 my-1 py-1 font-medium hover:bg-gray-600 rounded", href_ href]


prereqSec :: [[CourseInfo]] -> Html ()
prereqSec ps =
  sectionSec "prereqs" "Prerequisites" $
    if not (null ps)
      then do
        p_ [class_ "tracking-tight text-lg mb-4"] "Each of the below requirements must be satisfied:"
        div_ [class_ "lg:grid lg:grid-cols-3 lg:gap-3"] $ do
          forM_ ps $ \ors -> do
            div_ [class_ "items-center mb-2 sm:mb-1 border rounded-lg px-5 py-6 sm:p-4"] $ do
              p_ [class_ "text-sm text-gray-600 mb-2"] "One of:"
              forM_ ors $ \cs -> do
                div_ [class_ "mb-2"] $ do
                  h1_ [class_ "sm:text-lg font-bold sm:mb-1 flex-grow"] $ do
                    let cs' = B64.encode $ encodeUtf8 (course_i cs)
                    a_ [href_ ("/course/" <> cs'), class_ "text-teal-600 hover:bg-teal-200"] $ toHtml $ course_i cs
                  p_ [class_ "text-sm text-gray-600"] $ toHtml $ desc cs
      else do
        p_ [class_ "tracking-tight text-lg mb-4"] "This class has no prerequisites."

instrsSec :: SectionMap -> Html ()
instrsSec im = do
  sectionSec "instrs" "Instructors" $
    forM_ (sortOn (recInstrRank . snd) $ Map.toList im) $ \(sid, sinfo) -> do
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
            with (gpaToHtml $ Seascape.Data.Sparse.gpaAvg sinfo) [class_ " text-sm whitespace-no-wrap"]
            p_ [class_ "text-xs sm:text-sm text-gray-600"] $ "avg. GPA"

courseView :: CourseViewInfo -> Html ()
courseView cvi = defaultPartial (courseName cvi <> " - Seascape") $ do
  topHero cvi
  midNav
  prereqSec (prereqList cvi)
  instrsSec (instrMap cvi)
