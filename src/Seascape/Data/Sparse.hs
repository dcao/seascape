{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeApplications, TypeOperators #-}
module Seascape.Data.Sparse where

import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Vinyl (Rec(..), ElField(..), rapply, rmapX, ruple)
import Data.Vinyl.Functor (Compose(..))
import Data.Vinyl.Lens
import Lens.Micro.Extras
import Frames
import Frames.CSV
import Pipes ((>->))
import qualified Pipes.Prelude as P

declareColumn "Instr" ''Text
declareColumn "Course" ''Text
declareColumn "Term" ''Text
declareColumn "Enrolled" ''Int
declareColumn "Evals" ''Int
declareColumn "RecClass" ''Double
declareColumn "RecInstr" ''Double
declareColumn "Hours" ''Double
declareColumn "GpaExp" ''Double
declareColumn "GpaAvg" ''Double
declareColumn "CountExp" ''Int
declareColumn "CountAvg" ''Int

type SectionTerm = Record '[Instr, Course, Term, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type Section = Record '[Instr, Course, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
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
getTerms df = L.fold L.nub $ view term <$> df

aggByTermMap :: Frame SectionTerm -> AggMap
aggByTermMap df = grouped
  where
    section = ruple . (rcast :: SectionTerm -> Record '[Instr, Course])
    foldf :: Record '[Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg, CountExp, CountAvg]
          -> SectionTerm
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

aggByTerm :: Frame SectionTerm -> Frame Section
aggByTerm = aggMapToFrame . aggByTermMap

frameFromICs :: [(Text, Text)] -> AggMap -> Frame Section
frameFromICs ics dfm = toFrame $ fmap (\x@(i, c) -> i &: c &: (fromJust $ Map.lookup x dfm)) ics

frameLen :: Frame a -> Int
frameLen df = L.fold L.length df
