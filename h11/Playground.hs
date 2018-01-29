module Main where

import           Control.Applicative

(.+) = liftA2 (+)
(.*) = liftA2 (*)

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
-- (<*>) :: f (a -> b) -> f a -> f b  

pair :: Applicative f => f a -> f b -> f (a, b)
-- pair fa fb = (\x y -> (x, y)) <$> fa <*> fb
-- pair fa fb = (,) <$> fa <*> fb
-- (\x y -> (x, y)) -> f a -> f b <*> fb
-- ==> (a -> b -> (a, b)) <$> f a <*> f b
-- ==> f (b -> (a, b)) <*> f b 
-- ==> f (a, b)

pair = liftA2 (,) 


main = do 
  print $ ([4, 5] .* pure 2) .+ [6,1]
  print $ pair (Just 1) (Just 2)
  print $ pair (Nothing :: Maybe Integer) (Just 2)


