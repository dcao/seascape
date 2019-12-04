{-# LANGUAGE TypeApplications, OverloadedStrings #-}
module Seascape.Actions.Search where

import Data.Foldable (toList)
import Data.Ix
import Data.SearchEngine
import Data.Text (intercalate, splitOn, Text, toLower)
import Frames (Frame, rgetField)
import Seascape.Data.Sparse

data SectionField = FInstr | FCourse
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type SectionSearchEng = SearchEngine (Text, Text) (Text, Text) SectionField NoFeatures

sectionSearchEngine :: Frame Section -> SectionSearchEng
sectionSearchEngine df = insertDocs (toList $ (\r -> (rgetField @Instr r, rgetField @Course r)) <$> df) initialEngine
  where
    initialEngine = initSearchEngine sectionSearchCfg searchRankParams

execSearch :: SectionSearchEng -> Text -> [(Text, Text)]
execSearch e t = query e $ splitOn " " t

extractInstr :: Text -> [Text]
extractInstr x = [lx, intercalate " " (reverse sx)] ++ sx
  where
    lx = toLower x
    sx = splitOn ", " lx
    

extractCourse :: Text -> [Text]
extractCourse x = [lx, head (splitOn " " lx)]
  where
    lx = toLower x

sectionSearchCfg :: SearchConfig (Text, Text) (Text, Text) SectionField NoFeatures
sectionSearchCfg = SearchConfig
  { documentKey = id
  , extractDocumentTerms = extractTerms
  , transformQueryTerm = flip (const toLower)
  , documentFeatureValue = const noFeatures
  }
  where
    extractTerms (i, _) FInstr  = extractInstr i
    extractTerms (_, c) FCourse = extractCourse c

searchRankParams :: SearchRankParameters SectionField NoFeatures
searchRankParams = SearchRankParameters
  { paramK1 = 1.5
  , paramB = const 0.5
  , paramFieldWeights = const 1
  , paramFeatureWeights = noFeatures
  , paramFeatureFunctions = noFeatures
  , paramResultsetSoftLimit = 200
  , paramResultsetHardLimit = 400
  , paramAutosuggestPrefilterLimit  = 500
  , paramAutosuggestPostfilterLimit = 500
  }

