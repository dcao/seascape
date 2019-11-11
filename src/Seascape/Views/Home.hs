{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Home where

import Lucid
import Seascape.Views.Partials

homeView :: Html ()
homeView = defaultPartial "Seascape" $ do
  div_ [class_ "bg-teal-100 pt-16 pb-40"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-5xl font-sans tracking-tight"] "Your virtual counselor."
      p_ [class_ "text-xl text-serif mt-5 text-teal-600"] "Seascape provides utilities for members of the UCSD community for discovering insights and making informed course-related decisions."
