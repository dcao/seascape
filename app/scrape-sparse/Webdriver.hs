{-# LANGUAGE OverloadedStrings #-}
module Webdriver where

import Data.Aeson
import GHC.Stack
import Network.HTTP.Client
import Network.HTTP.Types.Method
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON

-- We need to make our own manager to modify manager settings
mgr :: IO Manager
mgr = newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 600000000 })

setTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setTimeout ms = noReturn $ doSessCommand methodPost "/timeouts" params
  where params = object [ (Data.Aeson..=) "pageLoad" ms ]
