{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Home where

import Lucid
import Seascape.Views.Partials
import Text.Printf

topHero :: Int -> Html () -> Html ()
topHero ln extra =
  div_ [class_ "bg-teal-100 md:pt-32 md:pb-64 pt-16 pb-16 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-2xl md:text-5xl font-light font-sans tracking-tight"] "Your virtual counselor."
      p_ [class_ "text-md md:text-xl font-serif mt-5 text-teal-600"] $ do
        "Search for information and statistics on "
        strong_ $ (toHtml :: String -> Html ()) $ printf "%d different sections" ln
        " of courses at UCSD."
        with defaultSearchBar [class_ " mt-12 "]
        p_ [class_ "mt-2 text-teal-600"] "Tip! Search multiple professor names (separated by spaces) to quickly compare them."
      extra

seascapeFeature :: String -> String -> Html () -> Html ()
seascapeFeature title desc btn = div_ [class_ "md:w-4/12 w-full px-4"] $ do
  h1_ [class_ "text-teal-100 font-semibold text-xl tracking-tight"] $ toHtml title
  p_ [class_ "text-teal-200 mt-3 mb-6 text-md"] $ toHtml desc
  with btn [class_ " mb-8 "]

whySeascape :: Html ()
whySeascape =
  div_ [class_ "bg-teal-500 pt-16 pb-16 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-2xl md:text-3xl font-medium text-gray-200 font-sans"] "Why Seascape?"
      p_ [class_ "text-lg font-serif mt-4 text-teal-200"] "Seascape provides increased insights into classes and provides tools to make dealing with courses and course registration easier."
    div_ [class_ "flex flex-row justify-center text-center container mx-auto mt-12 flex-wrap"] $ do
      seascapeFeature "The smartest CAPE" "Seascape takes averages for course sections over all quarters and makes predictions for future quarters based on this data, making it easier to decide if a course is worth taking." $
        a_ [href_ "/listing", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white"] "Course listing"
      seascapeFeature "Automated course scheduling" "Seascape can use its database of courses to automatically build a quarter schedule based on your preferences." $
        a_ [class_ "cursor-not-allowed inline-block text-sm px-4 py-2 leading-none border rounded text-gray-400 border-gray-400 cursor-not-allowed"] "Coming soon"
      seascapeFeature "Course plan generator" "Seascape understands UCSD course requirements, and can automatically generate a full college course plan based on your college, major, and fulfilled prerequisites." $
        -- a_ [href_ "/plan", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white"] "Course planner"
        a_ [class_ "cursor-not-allowed inline-block text-sm px-4 py-2 leading-none border rounded text-gray-400 border-gray-400 cursor-not-allowed"] "Coming soon"

contactUs :: Html ()
contactUs =
  div_ [class_ "bg-teal-100 flex flex-row flex-wrap"] $ do
    div_ [class_ "w-full md:w-1/2"] $ do
      img_ [class_ "w-full object-cover", style_ "height: 32rem;", src_ "/img/losers.jpg"]
    div_ [class_ "w-full md:w-1/2 flex flex-col justify-left text-left pt-16 pb-24 px-8 lg:pl-8 lg:pr-64"] $ do
      h1_ [class_ "text-2xl md:text-3xl font-medium text-gray-700 font-sans"] "Help us help you"
      p_ [class_ "text-lg font-serif mt-4 text-teal-600"] "Seascape is an open-source project created by a couple of undergrads here at UCSD. As a result, Seascape probably lacks useful features and contains lots of bugs. Feel free to file an issue with potential improvements or make a contribution yourself!"
      p_ [class_ "mt-6"] $ do
        a_ [href_ "https://github.com/dcao/seascape/issues/new", class_ "inline-block mr-2 text-sm px-4 py-2 leading-none border rounded text-black border-black hover:border-transparent hover:text-teal-100 hover:bg-black"] "File an issue"
        a_ [href_ "https://github.com/dcao/seascape", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-black border-black hover:border-transparent hover:text-teal-100 hover:bg-black"] "View Seascape on GitHub"

homeView :: Int -> Html () -> Html ()
homeView ln extra = defaultPartial "Seascape" $ do
  topHero ln extra
  whySeascape
  contactUs
