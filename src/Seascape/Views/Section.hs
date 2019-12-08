{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Seascape.Views.Section where

import Frames
import Lucid
import Seascape.Data.Sparse
import Seascape.Views.Partials

topHero :: Section -> Html ()
topHero s =
  div_ [class_ "bg-teal-100 pt-12 pb-8 px-6"] $ do
    div_ [class_ "flex flex-col justify-center max-w-2xl mx-auto text-center"] $ do
      h1_ [class_ "text-3xl font-medium font-sans"] $ toHtml $ rgetField @Instr s
      h1_ [class_ "text-2xl font-sans"] $ toHtml $ rgetField @Course s
    div_ [class_ "mx-auto max-w-4xl mt-4 flex flex-row justify-center"] $ do
      gauge (rgetField @RecInstr s) 100
      gauge (rgetField @RecClass s) 100
      gauge (rgetField @GpaAvg s) 4

sectionView :: Section -> Html ()
sectionView s = defaultPartial "Seascape" $ do
  topHero s
  p_ $ toHtml $ ("Hello world!" :: String)
