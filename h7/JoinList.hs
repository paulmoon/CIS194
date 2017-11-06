module JoinList where

import Data.Monoid
import Sized

-- m : monoidal annotations to the structure.
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

-- Exercise 1: Append 2 JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) left right = Append (mappend (tag left) (tag right)) left right

-- Returns the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

-- Exercise 2: indexJ

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- First attempt
-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- indexJ n joinList = let 
--   lst = jlToList joinList
  
--   (!!?) :: [a] -> Int -> Maybe a
--   []     !!? _         = Nothing
--   _      !!? i | i < 0 = Nothing
--   (x:xs) !!? 0         = Just x
--   (x:xs) !!? i         = xs !!? (i-1) 

--   in (!!?) lst n

-- Second attempt
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single size a) = if n == 0 then Just a else Nothing
indexJ n (Append _ left right) = 
  if n < getTagSize left
    then indexJ n left 
    else indexJ (n - getTagSize left) right

instance Sized a => Sized (JoinList a b) where
  size Empty = Size 0
  size (Single a b) = size a
  size (Append a left right) = size a + size left + size right

-- instance Num a => Sized (Product a) where
  -- size a = Size (fromIntegral $ getProduct a)

getTagSize :: (Monoid a, Sized a) => JoinList a b -> Int
getTagSize = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n joinList | n <= 0 = joinList 
                 | n >= getTagSize joinList = Empty
dropJ _ joinList@(Single _ _) = joinList
dropJ n (Append m left right)
  | n >= getTagSize left = dropJ (n - getTagSize left) right
  | otherwise = (dropJ n left) +++ right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n joinList | n <= 0 = Empty
                 | n >= getTagSize joinList = joinList
takeJ n joinList@(Single m a) = joinList
takeJ n (Append m left right)
  | n > getTagSize left = left +++ takeJ (n - getTagSize left) right
  | otherwise = takeJ n left

sampleJoinList = 
        Append (Size 4)
          (Append (Size 3)
            (Single (Size 1) 'y')
            (Append (Size 2)
              (Single (Size 1) 'e')
              (Single (Size 1) 'a')))
          (Single (Size 1) 'h')                  

-- main :: IO()
-- main = do 
--   print $ (Single (Size 2) 'a') +++ (Single (Size 3) 'b')

--   print $ indexJ 0 (Single (Size 1) 'a') == Just 'a'
--   print $ indexJ 1 (Single (Size 1) 'a') == Nothing

--   print $ dropJ 0 (Single (Size 1) 'a') == (Single (Size 1) 'a')
--   print $ dropJ 1 (Single (Size 1) 'a') == Empty
--   print $ dropJ 0 sampleJoinList == sampleJoinList
--   print $ dropJ 3 sampleJoinList == (Single (Size 1) 'h')
--   print $ dropJ 4 sampleJoinList == Empty

--   print $ takeJ 0 sampleJoinList == Empty
--   print $ takeJ 1 sampleJoinList == (Single (Size 1) 'y')
--   print $ takeJ 2 sampleJoinList == (Append (Size 2) (Single (Size 1) 'y') (Single (Size 1) 'e'))
--   print $ takeJ 4 sampleJoinList == sampleJoinList
--   print $ takeJ 5 sampleJoinList == sampleJoinList
