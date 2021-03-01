{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings, DuplicateRecordFields #-}
module Seascape.Data.Prereqs where

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data CourseInfo = CourseInfo
  { course_i :: Text
  , desc     :: Text
  } deriving (Show, Eq, Ord)

instance FromJSON CourseInfo where
  parseJSON (Object v) = CourseInfo
    <$> v .: "course"
    <*> v .: "desc"

-- A PrereqListing is a listing in the prereq file.
-- We have this to automate reading in the JSON.
data PrereqListing = PrereqListing
  { course_p :: Text
  , prereqs  :: [[CourseInfo]]
  } deriving (Show, Eq, Ord)

instance FromJSON PrereqListing where
  parseJSON (Object v) = PrereqListing
    <$> v .: "course"
    <*> v .: "prereqs"

-- A PrereqMap is a map from a course to a list of lists of courses and their
-- descriptions. The outer list is AND, the inner list is OR
type PrereqMap = Map.Map Text [[CourseInfo]]

readPrereqs :: String -> IO PrereqMap
readPrereqs loc = do
  d <- fromJust <$> (decodeFileStrict loc :: IO (Maybe [PrereqListing]))
  return $ xformPrereqs d

xformPrereqs :: [PrereqListing] -> PrereqMap
xformPrereqs = Map.fromList . fmap (\x -> (course_p x, prereqs x))
