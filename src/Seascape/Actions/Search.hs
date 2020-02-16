{-# LANGUAGE TypeApplications, OverloadedStrings #-}
module Seascape.Actions.Search where

import Control.Monad (join)
import Data.Ix
import qualified Data.Map.Strict as Map
import Data.SearchEngine
import Data.Char (isDigit)
import Data.Text (intercalate, splitOn, Text, toLower, filter, inits)
import Data.Monoid (mconcat)
import Seascape.Data.Sparse

data SectionField = FInstr | FCourse
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type SectionSearchEng = SearchEngine SectionID SectionID SectionField NoFeatures

sectionSearchEngine :: SectionMap -> SectionSearchEng
sectionSearchEngine dfm = insertDocs (Map.keys dfm) initialEngine
  where
    initialEngine = initSearchEngine sectionSearchCfg searchRankParams

execSearch :: SectionSearchEng -> Text -> [SectionID]
execSearch e t = query e $ splitOn " " t

execSearchExplain :: SectionSearchEng -> Text -> [(Explanation SectionField NoFeatures Text, SectionID)]
execSearchExplain e t = queryExplain e $ splitOn " " t

extractInstr :: Text -> [Text]
extractInstr x = [lx, intercalate " " (reverse sx)] ++ sx
  where
    lx = toLower x
    sx = join $ fmap inits $ join $ splitOn " " <$> splitOn ", " lx

extractCourse :: Text -> [Text]
extractCourse x = [lx, mconcat spl, Data.Text.filter isDigit x] ++ spl ++ [mconcat spl]
  where
    lx = toLower x
    spl = splitOn " " lx

sectionSearchCfg :: SearchConfig SectionID SectionID SectionField NoFeatures
sectionSearchCfg = SearchConfig
  { documentKey = id
  , extractDocumentTerms = extractTerms
  , transformQueryTerm = flip (const toLower)
  , documentFeatureValue = const noFeatures
  }
  where
    extractTerms (SectionID i _) FInstr  = extractInstr i
    extractTerms (SectionID _ c) FCourse = extractCourse c

searchRankParams :: SearchRankParameters SectionField NoFeatures
searchRankParams = SearchRankParameters
  { paramK1 = 1.5
  , paramB = paramB
  , paramFieldWeights = paramFieldWeights
  , paramFeatureWeights = noFeatures
  , paramFeatureFunctions = noFeatures
  , paramResultsetSoftLimit = 1000
  , paramResultsetHardLimit = 40000
  , paramAutosuggestPrefilterLimit  = 500
  , paramAutosuggestPostfilterLimit = 500
  }
  where
    paramB FInstr = 0.9
    paramB FCourse = 0.5

    paramFieldWeights FInstr = 3
    paramFieldWeights FCourse = 5

