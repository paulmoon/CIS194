{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module StringBuffer where

import Data.Monoid

import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

-- Exercise 4
getTagScore :: JoinList (Score, Size) String -> Int
getTagScore = scoreToInt . fst . tag

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  -- | Convert a buffer to a String.
  toString     = unlines . jlToList

  -- | Create a buffer from a String.
  fromString   = fromLine . lines where
                 fromLine [] = Empty
                 fromLine (xs:[]) = scoreSizeLine xs
                 fromLine xs = fromLine (take half xs) +++
                               fromLine (drop half xs)
                               where half = (length xs) `div` 2

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine _ _ Empty = Empty
  replaceLine n l b = 
    (takeJ n b) +++ scoreSizeLine l +++ (dropJ (n+1) b)

  -- | Compute the number of lines in the buffer.
  numLines     = getTagSize

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value        = getTagScore

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs