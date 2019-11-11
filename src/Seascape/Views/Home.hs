{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Home where

import Lucid
import Seascape.Views.Partials

homeView :: Html ()
homeView = defaultPartial "Seascape" $ do
  div_ [class_ "bg-teal-100 pt-16 pb-40"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-5xl font-sans tracking-tight"] "Your virtual counselor."
      p_ [class_ "text-xl text-serif mt-5 text-teal-600"] "Seascape provides utilities for members of the UCSD community to discover insights and make informed course-related decisions."
      -- input_ [class_ "shadow-lg outline-none appearance-none border rounded-lg w-full py-4 px-6 text-xl text-gray-700 leading-tight", id_ "username", type_ "text", placeholder_ "Search for a class..."]
      div_ [class_ "mt-12 flex items-center border shadow-lg rounded-lg w-full bg-white pl-6 text-xl leading-tight"] $ do
        input_ [class_ "outline-none appearance-none border-none w-full text-gray-700", id_ "username", type_ "text", placeholder_ "Search for a class or instructor"]
        button_ [class_ "flex-shrink-0 text-teal-500 hover:text-teal-700 p-6", type_ "button"] $ do
          i_ [class_ "fas fa-search"] mempty
