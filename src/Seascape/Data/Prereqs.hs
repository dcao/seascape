{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings, DuplicateRecordFields #-}
module Seascape.Data.Prereqs where

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.List
import Control.Monad.State.Lazy

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

-- buildGraph builds a graph of the dependencies of a course in
-- DOT format.
buildGraph :: PrereqMap -> Text -> Text
buildGraph ps c = head <> go 0 ps c <> foot
  where
    head = "digraph org {"
    foot = "}"

    go :: Int -> PrereqMap -> Text -> Text
    go cc ps c = concat $ fmap createEdges prereqs
      where
        prereqs = Map.lookup ps c

        createEdges :: [CourseInfo] -> Text
        createEdges dis = if length dis >= 2
          then ("subgraph cluster_" <> show cc <> " { style=filled; color=lightgrey; node [style=filled, color=white]; " <> (concat $ fmap (\ch -> course_i ch <> "; ") dis) <> "} "
            <> (concat $ fmap (\ch -> "\"" <> course_i c <> "\" -> \"" <> course_i ch <> "\"; ") dis)
            <> (concat $ fmap (\ch -> go (cc + 1) ps (course_i ch)) dis)
          else (concat $ fmap (\ch -> "\"" <> course_i c <> "\" -> \"" <> course_i ch <> "\"; ") dis)
            <> (concat $ fmap (\ch -> go cc ps (course_i ch)) dis)

