module Main where

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

-- instance Applicative Maybe where
--   pure              = Just
--   Nothing <*> _     = Nothing
--   _ <*> Nothing     = Nothing
--   Just f <*> Just x = Just (f x)

-- <*> = contextual application. :: f (a -> b) -> f a -> f b
-- <$> = lifting. :: (a -> b) -> f a -> f b

-- liftA2 h fa fb = (h `fmap` fa) <*> fb
-- fmap h fa <*> fb
-- fmap h fa <*> fb
-- (a -> (b -> c)) (f a) -> f (b -> c)
-- f (b -> c) <*> f b => f c

-- f `fmap` x === pure f <*> x

-- pure :: a -> f a
-- fmap :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c


m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"


main :: IO()
main = do
  print $ Employee <$> m_name1 <*> m_phone1
  print $ Employee <$> m_name1 <*> m_phone2
  print $ Employee <$> m_name2 <*> m_phone1
  print $ Employee <$> m_name2 <*> m_phone2 
