{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Partials where

import Data.Text
import Lucid
import Lucid.Base
import Text.Printf
import Seascape.Data.Sparse

path_ :: Term arg result => arg -> result
path_ = Lucid.Base.term "path"

circle_ :: Term arg result => arg -> result
circle_ = Lucid.Base.term "circle"

g_ :: Term arg result => arg -> result
g_ = Lucid.Base.term "g"

js_ :: Monad m => Text -> HtmlT m ()
js_ t = termWith "script" [src_ t] $ ""

text_ :: Term arg result => arg -> result
text_ = Lucid.Base.term "text"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

r_ :: Text -> Attribute
r_ = makeAttribute "r"

cx_ :: Text -> Attribute
cx_ = makeAttribute "cx"

x_ :: Text -> Attribute
x_ = makeAttribute "x"

y_ :: Text -> Attribute
y_ = makeAttribute "y"

textAnchor_ :: Text -> Attribute
textAnchor_ = makeAttribute "text-anchor"

dominantBaseline_ :: Text -> Attribute
dominantBaseline_ = makeAttribute "dominant-baseline"

fontSize_ :: Text -> Attribute
fontSize_ = makeAttribute "font-size"

cy_ :: Text -> Attribute
cy_ = makeAttribute "cy"

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

transform_ :: Text -> Attribute
transform_ = makeAttribute "transform"

gauge :: (Ord a, Show a, Fractional a) => a -> (a -> a) -> (a -> Text) -> Html ()
gauge val frf shf = div_ [class_ "w-24"] $ do
  svg_ [viewBox_ "0 0 120 120"] $ do
    g_ $ do
      circle_ [class_ ("stroke-current " <> col), r_ "56", cx_ "60", cy_ "60", transform_ "rotate(-90 60 60)", style_ ("fill: transparent; stroke-width: 3; stroke-dasharray: " <> pack (show $ frac * 352) <> ", 352;")] ""
      text_ [class_ ("text-2xl fill-current " <> col), x_ "50%", y_ "50%", dominantBaseline_ "middle", textAnchor_ "middle"] $ toHtml $ shf val

  where
    frac = frf val
    col' f
      | f >= 0.85 = "text-teal-700"
      | f >= 0.70 = "text-orange-700"

      | otherwise = "text-red-700"
    col :: Text
    col = col' frac

errAlert :: Text -> Html ()
errAlert t =
  div_ [class_ "bg-orange-100 border-l-4 border-orange-500 text-orange-700 p-4", role_ "alert"] $ do
    p_ $ do
      strong_ "Error! "
      toHtml t

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
        a_ [href_ "/listing", class_"block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Course Listing"
        -- a_ [href_ "/plan", class_"block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Course Planner"
      div_ $ do
        a_ [href_ "https://github.com/dcao/seascape", class_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white mt-4 lg:mt-0"] "Github"

defaultPartial :: Text -> Html () -> Html ()
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
        toHtmlRaw ("&copy; 2020 David Cao, Tung Doan. Created at " :: Text)
        a_ [href_ "https://sites.google.com/a/eng.ucsd.edu/spis/", class_ "text-teal-600"] "SPIS 2019."
      script_ "(function() {var script = document.createElement('script'); window.counter = 'https://seascape.goatcounter.com/count'; script.async = 1; script.src = '//gc.zgo.at/count.js'; var ins = document.getElementsByTagName('script')[0]; ins.parentNode.insertBefore(script, ins);})();"

searchBar :: Text -> SearchOrdering -> Html ()
searchBar t r = form_ [action_ "/listing"] $ do
  div_ [class_ "flex items-center border shadow-lg rounded-lg w-full bg-white text-xl leading-tight"] $ do
    input_ [type_ "text", name_ "q", class_ "pl-6 outline-none appearance-none border-none w-full text-gray-700", value_ t, id_ "username", type_ "text", placeholder_ "Type a class or instructor here!"]
    select_ [name_ "sortBy", class_ "appearance-none block bg-white px-2 text-lg text-teal-500 focus:outline-none text-underline"] $ do
      option_ (optSel Relevance "relevance") "Sort by relevance"
      option_ (optSel Ranking "ranking") "Sort by ranking"
    div_ [class_ "pointer-events-none flex items-center text-teal-600"] $
      svg_ [class_ "fill-current h-4 w-4", viewBox_ "0 0 20 20"] $
        path_ [d_ "M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z"] $ mempty
    button_ [class_ "flex-shrink-0 text-teal-500 hover:text-teal-700 p-6", type_ "submit"] $ do
      i_ [class_ "fas fa-search"] mempty
  where
    optSel od v
      | od == r    = [value_ v, selected_ "selected"]
      | otherwise = [value_ v]

defaultSearchBar :: Html ()
defaultSearchBar = searchBar "" Relevance

sectionHero :: Html () -> Html ()
sectionHero body =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] body

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

hrsToHM :: Double -> (Int, Int)
hrsToHM x = floor . (*) 60 <$> properFraction x

timeFmt :: Double -> String
timeFmt d =
  let (h, m) = hrsToHM d in
  (printf "%d:%02d" h m :: String)

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

gpaToHtml :: Gpa -> Html ()
gpaToHtml (Gpa Nothing) = h1_ [class_ "font-medium sm:text-lg font-mono text-gray-500"] "N/A"
gpaToHtml (Gpa (Just (_, x))) =
  h1_ [class_ "font-medium sm:text-lg font-mono"] $ do
    toHtml $ gpaToLetter x
    span_ [class_ "text-gray-700"] $ toHtml $ " (" <> roundToStr 2 x <> ")"

sectionSec :: Text -> Html () -> Html () -> Html ()
sectionSec tid title rest =
  div_ [id_ tid, class_ "flex flex-col pb-6 mx-auto max-w-5xl"] $ do
    span_ [class_ "text-sm tracking-widest uppercase px-2 py-1 bg-gray-300 mr-auto mb-4 rounded mb-3"] title
    rest
