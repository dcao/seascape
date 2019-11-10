{-# LANGUAGE OverloadedStrings #-}
module Seascape.Views.Partials where

import Lucid

defaultPartial :: String -> Html () -> Html ()
defaultPartial t body =
  html_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      title_ $ toHtml t
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      link_ [rel_ "stylesheet", href_ "/css/style.css"]
    body_ [class_ "font-sans" ] $ do
      body
