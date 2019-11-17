{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Partials where

import Data.Text
import Lucid
import Lucid.Base

d_ :: Text -> Attribute
d_ = makeAttribute "d"

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

path_ :: Term arg result => arg -> result
path_ = term "path"

navbarPartial :: Html ()
navbarPartial = nav_ [class_ "flex items-center justify-between flex-wrap bg-teal-500 py-5 shadow-lg z-100 px-6 relative"] $ do
  div_ [class_ "mx-auto container w-full justify-between flex-wrap flex items-center"] $ do
    div_ [class_ "flex items-center flex-shrink-0 text-white"] $ do
      span_ [class_ "font-semibold text-xl tracking-tight"] "Seascape"
    div_ [class_ "block lg:hidden"] $ do
      button_ [class_ "flex items-center px-3 py-2 border rounded text-teal-200 border-teal-400 hover:text-white hover:border-white"] $ do
        svg_ [class_ "fill-current h-3 w-3", viewBox_ "0 0 20 20", xmlns_ "http://www.w3.org/2000/svg"] $ do
          title_ "Menu"
          path_ [d_ "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"] mempty
    div_ [class_ "w-full block flex-grow lg:flex lg:items-center lg:w-auto"] $ do
      div_ [class_ "text-md lg:mx-auto"] $ do
        a_ [href_ "/", class_"block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Home"
        a_ [href_ "#", class_"block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Course Listing"
      div_ $ do
        a_ [href_ "https://github.com/dcao/seascape", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white mt-4 lg:mt-0"] "Github"

defaultPartial :: String -> Html () -> Html ()
defaultPartial t body =
  html_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      title_ $ toHtml t
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      link_ [rel_ "stylesheet", href_ "/css/style.css"]
    body_ [class_ "font-sans"] $ do
      navbarPartial
      body
    p_ [class_ "text-center text-gray-500 text-xs mt-8 mb-8"] $ do
      toHtmlRaw ("&copy; 2019 David Cao, Tung Doan. Born at " :: Text)
      a_ [href_ "https://sites.google.com/a/eng.ucsd.edu/spis/", class_ "text-teal-600"] "SPIS 2019."
