{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Home where

import Lucid
import Seascape.Views.Partials

-- expand top?
-- why seascape - features
-- report bugs - github link - contact?
-- about us

topHero :: Html ()
topHero =
  div_ [class_ "bg-teal-100 pt-32 pb-64 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-5xl font-light font-sans tracking-tight"] "Your virtual counselor."
      p_ [class_ "text-xl font-serif mt-5 text-teal-600"] "Seascape provides utilities for members of the UCSD community to discover insights and make informed course-related decisions."
      -- input_ [class_ "shadow-lg outline-none appearance-none border rounded-lg w-full py-4 px-6 text-xl text-gray-700 leading-tight", id_ "username", type_ "text", placeholder_ "Search for a class..."]
      div_ [class_ "mt-12 flex items-center border shadow-lg rounded-lg w-full bg-white pl-6 text-xl leading-tight"] $ do
        input_ [class_ "outline-none appearance-none border-none w-full text-gray-700", id_ "username", type_ "text", placeholder_ "Search for a class or instructor"]
        button_ [class_ "flex-shrink-0 text-teal-500 hover:text-teal-700 p-6", type_ "button"] $ do
          i_ [class_ "fas fa-search"] mempty

seascapeFeature :: String -> String -> Html () -> Html ()
seascapeFeature title desc btn = div_ [class_ "md:w-4/12 w-full px-4"] $ do
  h1_ [class_ "text-teal-100 font-semibold text-xl tracking-tight"] $ toHtml title
  p_ [class_ "text-teal-200 mt-3 mb-6 text-md"] $ toHtml desc
  with btn [class_ " mb-8 "]

whySeascape :: Html ()
whySeascape =
  div_ [class_ "bg-teal-500 pt-16 pb-16 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-3xl font-medium text-gray-200 font-sans"] "Why Seascape?"
      p_ [class_ "text-lg font-serif mt-4 text-teal-200"] "Seascape provides increased insights into classes and provides tools to make dealing with courses and course registration easier."
    div_ [class_ "flex flex-row justify-center text-center container mx-auto mt-12 flex-wrap"] $ do
      seascapeFeature "The smartest CAPE" "Seascape takes averages for course sections over all quarters and makes predictions for future quarters based on this data, making it easier to decide if a course is worth taking." $
        a_ [href_ "/course", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white"] "Course listing"
      seascapeFeature "Automated course scheduling" "Seascape can use its database of courses to automatically build a quarter schedule based on your preferences." $
        a_ [class_ "cursor-not-allowed inline-block text-sm px-4 py-2 leading-none border rounded text-gray-400 border-gray-400 cursor-not-allowed"] "Coming soon"
      seascapeFeature "College plan generator" "Seascape understands UCSD course requirements, and can automatically generate a full college course plan based on your college, major, and fulfilled prerequisites." $
        a_ [class_ "cursor-not-allowed inline-block text-sm px-4 py-2 leading-none border rounded text-gray-400 border-gray-400 cursor-not-allowed"] "Coming soon"

contactUs :: Html ()
contactUs =
  div_ [class_ "bg-teal-100 flex flex-row flex-wrap"] $ do
    div_ [class_ "w-full md:w-1/2"] $ do
      img_ [class_ "w-full object-cover", style_ "height: 32rem;", src_ "/img/losers.jpg"]
    div_ [class_ "w-full md:w-1/2 flex flex-col justify-left text-left pt-16 pb-24 px-8 lg:pl-8 lg:pr-64"] $ do
      h1_ [class_ "text-3xl font-medium text-gray-700 font-sans"] "Help us help you"
      p_ [class_ "text-lg font-serif mt-4 text-teal-600"] "Seascape is an open-source project created by a couple of undergrads here at UCSD. As a result, Seascape probably lacks useful features and contains lots of bugs. Feel free to file an issue with potential improvements or make a contribution yourself!"
      p_ [class_ "mt-6"] $ do
        a_ [href_ "https://github.com/dcao/seascape/issues/new", class_ "inline-block mr-2 text-sm px-4 py-2 leading-none border rounded text-black border-black hover:border-transparent hover:text-teal-100 hover:bg-black"] "File an issue"
        a_ [href_ "https://github.com/dcao/seascape", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-black border-black hover:border-transparent hover:text-teal-100 hover:bg-black"] "View Seascape on GitHub"

homeView :: Html ()
homeView = defaultPartial "Seascape" $ do
  topHero
  whySeascape
  contactUs
