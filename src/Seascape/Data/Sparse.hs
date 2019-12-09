{-# LANGUAGE BangPatterns, DataKinds, DeriveGeneric, FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards, TemplateHaskell, TypeApplications, TypeOperators #-}
module Seascape.Data.Sparse where

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vinyl (Rec(..), ElField(..), rapply, rmapX, ruple)
import Data.Vinyl.Functor (Compose(..))
import Data.Vinyl.Lens
import Lens.Micro.Extras
import Frames
import Frames.CSV
import GHC.Generics
import Pipes ((>->))
import qualified Pipes.Prelude as P

declareColumn "Instr" ''Text
declareColumn "Course" ''Text
declareColumn "Term" ''Text
declareColumn "TermIx" ''Int
declareColumn "Enrolled" ''Int
declareColumn "Evals" ''Int
declareColumn "RecClass" ''Double
declareColumn "RecInstr" ''Double
declareColumn "RecInstrRank" ''Int
declareColumn "Hours" ''Double
declareColumn "GpaExp" ''Double
declareColumn "GpaAvg" ''Double
declareColumn "CountExp" ''Int
declareColumn "CountAvg" ''Int

type SectionTerm = Record '[Instr, Course, Term, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type SectionTermIx = Record '[Instr, Course, Term, TermIx, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type Section = Record '[Instr, Course, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type SectionAgg = Record '[Instr, Course, Enrolled, Evals, RecClass, RecInstr, RecInstrRank, Hours, GpaExp, GpaAvg]
type AggMap = Map.Map (Text, Text) (Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg])

-- Janky, for testing
instance Show a => Show (Frame a) where
  show f@(Frame l _) = "df of len " ++ show l ++ "\n" ++ intercalate "\n" (show <$> L.fold L.list f)

defaultDataLoc :: String
defaultDataLoc = "data/data.csv"

defaultRow :: Rec (First :. ElField) (RecordColumns SectionTerm)
defaultRow = Compose (First (Just (Field "")))
          :& Compose (First (Just (Field "")))
          :& Compose (First (Just (Field "")))
          :& Compose (First (Just (Field 0)))
          :& Compose (First (Just (Field 0)))
          :& Compose (First (Just (Field 0)))
          :& Compose (First (Just (Field 0)))
          :& Compose (First (Just (Field 0)))
          :& Compose (First (Just (Field (-1))))
          :& Compose (First (Just (Field (-1))))
          :& RNil

loadFrame :: String -> IO (Frame SectionTerm)
loadFrame s = inCoreAoS sdf
  where
    holeFiller = recMaybe . rmapX @(First :. ElField) getFirst
               . rapply (rmapX @(First :. ElField) (flip mappend) defaultRow)
               . rmapX @_ @(First :. ElField) First
    sdf = (readTableMaybeOpt (defaultParser { headerOverride = Nothing }) s) >-> P.map (fromJust . holeFiller)

getTerms :: Frame SectionTerm -> [Text]
getTerms df = reverse $ L.fold L.nub $ view term <$> df

loadFrameIx :: String -> IO (Frame SectionTermIx)
loadFrameIx f = do
  df <- loadFrame f
  let terms = getTerms df
  let m r = rgetField @Instr r
         &: rgetField @Course r
         &: rgetField @Term r
         &: fromJust (elemIndex (rgetField @Term r) terms)
         &: rgetField @Enrolled r
         &: rgetField @Evals r
         &: rgetField @RecClass r
         &: rgetField @RecInstr r
         &: rgetField @Hours r
         &: rgetField @GpaExp r
         &: rgetField @GpaAvg r
         &: RNil
  return $ m <$> df

aggByTermMap :: Frame SectionTermIx -> AggMap
aggByTermMap df = grouped
  where
    section = ruple . (rcast :: SectionTermIx -> Record '[Instr, Course])
    foldf :: Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg, CountExp, CountAvg]
          -> SectionTermIx
          -> Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg, CountExp, CountAvg]
    foldf acc x = rgetField @Enrolled acc + rgetField @Enrolled x
               &: rgetField @Evals acc + rgetField @Evals x
               &: rgetField @RecClass acc + (rgetField @RecClass x * (fromIntegral (rgetField @Evals x)))
               &: rgetField @RecInstr acc + (rgetField @RecInstr x * (fromIntegral (rgetField @Evals x)))
               &: rgetField @Hours acc + (rgetField @Hours x * (fromIntegral (rgetField @Evals x)))
               &: (if a /= -1 then rgetField @GpaExp acc + (rgetField @GpaExp x * (fromIntegral (rgetField @Evals x))) else rgetField @GpaExp acc)
               &: (if b /= -1 then rgetField @GpaAvg acc + (rgetField @GpaAvg x * (fromIntegral (rgetField @Evals x))) else rgetField @GpaAvg acc)
               &: (if a /= -1 then rgetField @CountExp acc + (fromIntegral (rgetField @Evals x)) else rgetField @CountExp acc)
               &: (if b /= -1 then rgetField @CountAvg acc + (fromIntegral (rgetField @Evals x)) else rgetField @CountAvg acc)
               &: RNil
      where
        a = rgetField @GpaExp x
        b = rgetField @GpaAvg x

    convr :: Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg, CountExp, CountAvg]
          -> Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
    convr acc = rgetField @Enrolled acc
             &: rgetField @Evals acc
             &: (rgetField @RecClass acc) / cnt
             &: (rgetField @RecInstr acc) / cnt
             &: (rgetField @Hours    acc) / cnt
             &: (if cntE /= 0 then (rgetField @GpaExp   acc) / cntE else -1)
             &: (if cntA /= 0 then (rgetField @GpaAvg   acc) / cntA else -1)
             &: RNil
      where
        cnt  = fromIntegral $ rgetField @Evals acc
        cntE = fromIntegral $ rgetField @CountExp acc
        cntA = fromIntegral $ rgetField @CountAvg acc

    def :: Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg, CountExp, CountAvg]
    def = 0 &: 0 &: 0 &: 0 &: 0 &: 0 &: 0 &: 0 &: 0 &: RNil

    grouped = L.fold (L.groupBy section (L.Fold foldf def convr)) df
  
aggMapToFrame :: AggMap -> Frame Section
aggMapToFrame = toFrame . Map.mapWithKey (\(i, c) v -> i &: c &: v)

aggByTerm :: Frame SectionTermIx -> Frame Section
aggByTerm = aggMapToFrame . aggByTermMap

frameFromICs :: [(Text, Text)] -> AggMap -> Frame Section
frameFromICs ics dfm = toFrame $ fmap (\x@(i, c) -> i &: c &: (fromJust $ Map.lookup x dfm)) ics

frameLen :: Frame a -> Int
frameLen df = L.fold L.length df

genSectionAgg :: Frame Section -> Frame SectionAgg
genSectionAgg df = f <$> df
  where
    dfl = sortBy (\a b -> compare (rgetField @RecInstr b) (rgetField @RecInstr a)) $ L.fold L.list df
    dfm = Map.fromList $ (\(i, x) -> ((rgetField @Instr x, rgetField @Course x), i + 1)) <$> zip [0..] dfl
    f r = rgetField @Instr r
       &: rgetField @Course r
       &: rgetField @Enrolled r
       &: rgetField @Evals r
       &: rgetField @RecClass r
       &: rgetField @RecInstr r
       &: (fromJust $ Map.lookup (rgetField @Instr r, rgetField @Course r) dfm)
       &: rgetField @Hours r
       &: rgetField @GpaExp r
       &: rgetField @GpaAvg r
       &: RNil
    
-- For JSON
data SectionTermIxT = SectionTermIxT
  { st_instr :: Text
  , st_course :: Text
  , st_term :: Text
  , st_termIx :: Int
  , st_enrolled :: Int
  , st_evals :: Int
  , st_recClass :: Double
  , st_recInstr :: Double
  , st_hours :: Double
  , st_gpaExp :: Double
  , st_gpaAvg :: Double
  } deriving (Generic, Show)
   
instance FromJSON SectionTermIxT
instance ToJSON SectionTermIxT

structifySec :: SectionTermIx -> SectionTermIxT
structifySec r = SectionTermIxT { .. }
  where
    st_instr = rgetField @Instr r
    st_course = rgetField @Course r
    st_term = rgetField @Term r
    st_termIx = rgetField @TermIx r
    st_enrolled = rgetField @Enrolled r
    st_evals = rgetField @Evals r
    st_recClass = rgetField @RecClass r
    st_recInstr = rgetField @RecInstr r
    st_hours = rgetField @Hours r
    st_gpaExp = rgetField @GpaExp r
    st_gpaAvg = rgetField @GpaAvg r

jsonifyFrame :: Frame SectionTermIx -> Text
jsonifyFrame df = decodeUtf8 $ toStrict $ encode $ toJSON $ L.fold L.list $ fmap structifySec df
