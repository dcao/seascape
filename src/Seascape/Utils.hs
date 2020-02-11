module Seascape.Utils where

import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Map.Strict as Map

groupBy' :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy' key = Map.fromListWith (++) . map (key &&& pure)

median :: (Ord a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median x =
   if odd n
     then Just $ sort x !! (n `div` 2)
     else Just $ ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x
