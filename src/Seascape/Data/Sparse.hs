{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeApplications, TypeOperators #-}
module Seascape.Data.Sparse where

import qualified Data.Foldable as F
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Vinyl (Rec(RNil))
import Data.Vinyl.Lens
import Data.Vinyl.XRec (toHKD)
import Lens.Micro
import Lens.Micro.Extras
import Frames
import Frames.CSV
import Frames.TH
import Pipes (Producer, (>->))
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

type SectionQtr = Record '[Instr, Course, Term, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type Section = Record '[Instr, Course, Enrolled, Evals, RecClass, RecInstr, Hours, GpaExp, GpaAvg]
type SRow rc = Rec (Maybe :. ElField) (RecordColumns rc)
type SFrame m rc = Producer (SRow rc) m ()

-- Janky, for testing
instance (Show a) => Show (Frame a) where
  show df = show $ F.length df

defaultDataLoc :: String
defaultDataLoc = "data/data.csv"

loadRows :: MonadSafe m => String -> SFrame m SectionQtr
loadRows = readTableMaybeOpt (defaultParser { headerOverride = Nothing })

getTerms :: Monad m => SFrame m SectionQtr -> m [Text]
getTerms df = nub <$> P.toListM (df >-> P.map (fromJust . toHKD . rget @Term))

recByQtr :: Monad m => SFrame m SectionQtr -> (Text, Text) -> m Section
recByQtr df (i, c) = do
  enrolled' <- foldSum $ rows >-> P.map (fromJust . toHKD . rget @Enrolled)
  evals' <- foldSum $ rows >-> P.map (fromJust . toHKD . rget @Evals)
  recClass' <- foldAvg $ rows >-> P.map (fromJust . toHKD . rget @RecClass)
  recInstr' <- foldAvg $ rows >-> P.map (fromJust . toHKD . rget @RecInstr)
  hours' <- foldAvg $ rows >-> P.map (fromJust . toHKD . rget @Hours)
  gpaExp' <- foldAvg $ rows >-> P.map (toHKD . rget @GpaExp) >-> P.filter isJust >-> P.map fromJust
  gpaAvg' <- foldAvg $ rows >-> P.map (toHKD . rget @GpaAvg) >-> P.filter isJust >-> P.map fromJust
  return $ i &: c &: enrolled' &: evals' &: recClass' &: recInstr' &: hours' &: gpaExp' &: gpaAvg' &: RNil
  
  where
    filterMatch :: SRow SectionQtr -> Bool
    filterMatch r = ((fromJust $ toHKD $ rget @Instr r) == i) && ((fromJust $ toHKD $ rget @Course r) == c)

    foldSum = P.fold (+) 0 id
    avgTup (a, b) = a / b
    foldAvg r = P.fold (\(!a, !n) x -> (a + x, n + 1)) (0,0) avgTup r

    rows = df >-> P.filter filterMatch

-- TODO: Convert this to streaming? P.mapM
aggByQtr :: Monad m => SFrame m SectionQtr -> m (Frame Section)
aggByQtr df = do
  sections <- getSections
  toFrame <$> mapM (recByQtr df) sections
  
  where
    getSectionRec :: SRow SectionQtr -> SRow (Record '[Instr, Course])
    getSectionRec = rcast
    getSectionTup r = let x = getSectionRec r in (fromJust $ toHKD $ rget @Instr x, fromJust $ toHKD $ rget @Course x)
    getSections = nub <$> P.toListM (df >-> P.map getSectionTup)
