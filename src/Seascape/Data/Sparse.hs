{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings #-}
module Seascape.Data.Sparse where

import Data.Aeson hiding ((.:))
import qualified Data.ByteString.Lazy as BS
import Data.Csv hiding ((.=))
import qualified Data.Map.Strict as Map
import Data.List (nub, elemIndex, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Vector as V

data SectionID = SectionID
  { instr :: Text
  , course :: Text
  } deriving (Show, Eq, Ord)

data SectionInfo ix rank = SectionInfo
  { term :: !Text
  , termIx :: !ix
  , enrolled :: !Int
  , evals :: !Int
  , recClass :: !Double
  , recInstr :: !Double
  , recInstrRank :: !rank
  , hours :: !Double
  , gpaExp :: !Gpa
  , gpaAvg :: !Gpa
  } deriving (Show, Eq, Ord)

newtype Section ix rank = Section { unSection :: (SectionID, SectionInfo ix rank) }
  deriving (Show, Eq, Ord)
  
newtype Gpa = Gpa (Maybe (Int, Double))
  deriving (Eq, Ord)

gpaExists :: Gpa -> Bool
gpaExists (Gpa x) = isJust x

lossyGpa :: Gpa -> Double
lossyGpa (Gpa Nothing)       = ((-1) :: Double)
lossyGpa (Gpa (Just (_, x))) = x

instance Show Gpa where
  show (Gpa x) = show x

instance Semigroup Gpa where
  Gpa (Just (ca, a)) <> Gpa (Just (cb, b)) = Gpa $ Just $ (ca + cb, (a * fromIntegral ca + b * fromIntegral cb) / fromIntegral (ca + cb))
  Gpa (Just (ca, a)) <> Gpa Nothing        = Gpa $ Just (ca, a)
  Gpa Nothing        <> Gpa (Just (cb, b)) = Gpa $ Just (cb, b)
  Gpa Nothing        <> Gpa Nothing        = Gpa Nothing

-- This Semigroup intance is used for aggregating classes with the same SectionID.
instance Semigroup a => Semigroup (SectionInfo Int a) where
  a <> b = SectionInfo
    { term = getTerm $ compare (termIx a) (termIx b)
    , termIx = max (termIx a) (termIx b)
    , enrolled = (enrolled a) + (enrolled b)
    , evals = (evals a) + (evals b)
    , recClass = ((fromIntegral (evals a) * recClass a) + (fromIntegral (evals b) * recClass b)) / fromIntegral (evals a + evals b)
    , recInstr = ((fromIntegral (evals a) * recInstr a) + (fromIntegral (evals b) * recInstr b)) / fromIntegral (evals a + evals b)
    , recInstrRank = (recInstrRank a) <> (recInstrRank b)
    , hours = ((fromIntegral (evals a) * hours a) + (fromIntegral (evals b) * hours b)) / fromIntegral (evals a + evals b)
    , gpaExp = (gpaExp a) <> (gpaExp b)
    , gpaAvg = (gpaAvg a) <> (gpaAvg b)
    }
    where
      getTerm LT = term a
      getTerm _  = term b

type SectionMap = Map.Map SectionID (SectionInfo Int Int)

-- Cassava stuff
instance FromNamedRecord (Section () ()) where
  parseNamedRecord m = fmap Section $ (,) <$> secid <*> secinfo
    where
      secid = SectionID <$> m .: "instr" <*> m .: "course"
      getGpa (Just x) = fmap (Gpa . Just) $ (,) <$> m .: "evals" <*> return x
      getGpa Nothing  = return $ Gpa Nothing
      secinfo = SectionInfo
        <$> m .: "term"
        <*> return ()
        <*> m .: "enrolled"
        <*> m .: "evals"
        <*> m .: "recClass"
        <*> m .: "recInstr"
        <*> return ()
        <*> m .: "hours"
        <*> (getGpa =<< m .: "gpaExp")
        <*> (getGpa =<< m .: "gpaAvg")

instance ToJSON Gpa where
  toJSON = toJSON . lossyGpa

-- Aeson stuff
instance ToJSON (Section Int ()) where
  toJSON (Section (i, c)) =
    object [ "st_instr" .= instr i
           , "st_course" .= course i
           , "st_term" .= term c
           , "st_termIx" .= termIx c
           , "st_enrolled" .= enrolled c
           , "st_evals" .= evals c
           , "st_recClass" .= recClass c
           , "st_recInstr" .= recInstr c
           , "st_hours" .= hours c
           , "st_gpaExp" .= gpaExp c
           , "st_gpaAvg" .= gpaAvg c
           ]

rankBy :: (Ord a, Ord k) => (k -> v -> a) -> Map.Map k v -> Map.Map k Int
rankBy f m = Map.fromList $ zip (fst <$> sorted) [(1 :: Int)..]
  where
    sorted = sortBy (\a b -> compare ((uncurry f) b) ((uncurry f) a)) $ Map.toList m

genTermIx :: [Section () ()] -> [Section Int ()]
genTermIx entries = fmap (Section . (addTermIx <$>) . unSection) entries
  where
    terms = nub $ (term . snd . unSection) <$> entries

    addTermIx :: SectionInfo () () -> SectionInfo Int ()
    addTermIx info = info { termIx = fromJust (elemIndex (term info) terms) }

genSectionMap :: [Section Int ()] -> SectionMap
genSectionMap entries = pass2 . pass1 $ entries
  where
    -- First pass: aggregate by SectionID.
    pass1 :: [Section Int ()] -> Map.Map SectionID (SectionInfo Int ())
    pass1 = Map.fromListWith (<>) . fmap unSection

    -- Second pass: add recInstrRank.
    pass2 :: Map.Map SectionID (SectionInfo Int ()) -> Map.Map SectionID (SectionInfo Int Int)
    pass2 xs = mpRecInstr
      where
        recInstrRankMap = rankBy (\_ i -> recInstr i) xs
        mpRecInstr = Map.mapWithKey (\k x -> x { recInstrRank = fromJust $ Map.lookup k recInstrRankMap }) xs

readSections :: String -> IO (Either String [Section Int ()])
readSections loc = do
  f <- BS.readFile loc
  return $ (genTermIx . V.toList . snd) <$> decodeByName f

data SearchOrdering = Relevance | Ranking
  deriving (Eq)

defaultOrdering :: SearchOrdering
defaultOrdering = Relevance

getOrdering :: Maybe Text -> SearchOrdering
getOrdering (Just "ranking") = Ranking
getOrdering _                = Relevance

ordf :: SearchOrdering -> (Int, Section Int Int) -> Int
ordf Relevance (ord, _)                = ord
ordf Ranking   (_, Section (_, sinfo)) = recInstrRank sinfo

addCourseKey :: [Text] -> (Int, Section Int Int) -> ((Int, Text), (Int, Section Int Int))
addCourseKey cs a@(_, Section (sid, _)) = ((fromJust $ elemIndex c cs, c), a)
  where
    c = course sid

addSectionOrder :: SectionMap
                -> ((Int, Section Int Int) -> Int)
                -> (Int, SectionID)
                -> (Int, Section Int Int)
addSectionOrder sm orf (ord, x) =
  let sec = Section (x, fromJust $ Map.lookup x sm) in (orf (ord, sec), sec)
