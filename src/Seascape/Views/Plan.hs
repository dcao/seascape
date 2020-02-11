{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Plan where

import Lucid
import Seascape.Views.Partials

topHero :: Html ()
topHero =
  sectionHero $ do
    h1_ [class_ "text-3xl font-medium font-sans"] "Course planner"
    p_ [class_ "text-lg font-serif mt-3 text-teal-600"] "Generate a full plan of every course you need to take to graduate at UCSD."

body :: Html ()
body =
  div_ [class_ "px-6 mt-6"] $ do
    basicInfo

basicInfo :: Html ()
basicInfo =
  sectionSec "#basic-info" "Basic Info" $ do
    p_ [class_ "text-xl tracking-tight"] "You're not supposed to be here. This pag is under construction and will be finished shortly! You have been reported to the Academic Integrity Office."

planView :: Html ()
planView = defaultPartial "Course Planner - Seascape" $ do
  topHero
  body
