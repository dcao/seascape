{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Plan where

import Lucid
import Seascape.Views.Partials

topHero :: Html ()
topHero =
  sectionHero $ do
    h1_ [class_ "text-3xl font-medium font-sans"] "Course planner"
    p_ [class_ "text-lg font-serif mt-3 text-teal-600"] "Generate a full plan of every course you need to take to graduate at UCSD."

planView :: Html ()
planView = defaultPartial "Course Planner - Seascape" $ do
  topHero
