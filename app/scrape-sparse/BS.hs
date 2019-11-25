module BS where

import Data.ByteString
import Data.ByteString.Char8 as C
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Char
import Data.Word

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(PS x s l)
  | C.null ps = 0
  | f (unsafeLast ps) = l
  | otherwise = findFromEndUntil f (PS x s (l - 1))

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f ps = unsafeTake (findFromEndUntil (not . f . w2c) ps) ps
{-# INLINE dropWhileEnd #-}

dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps = C.take (C.length ps - n) ps

bstrip :: ByteString -> ByteString
bstrip = dropWhileEnd isSpace . C.dropWhile isSpace
