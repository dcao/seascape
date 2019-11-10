{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Seascape.Model where

import Database.Persist.TH

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User json
--     name String
-- |]
-- 
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Event json
--     name String
--     description String
-- |]
-- 
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Group json
--     name String
--     description String
-- |]
