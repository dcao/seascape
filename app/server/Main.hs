{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Seascape.App

main :: IO ()
main = do
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  let prt = maybe 8080 id herokuPort
  runSpock prt (spock spockCfg app)

-- main :: IO ()
-- main = putStrLn =<< (show . aggByTerm) <$> loadFrame "data/data.csv"
