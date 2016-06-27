type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a c

main :: IO()
main = do
  print $ hanoi 2 "a" "b" "c"