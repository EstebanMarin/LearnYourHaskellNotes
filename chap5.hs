-- higher order functions
-- generalizes zipping
-- zip' _ [] = []
-- zip' [] _ = []
-- zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) =
  f x y : zipWith' f xs ys

-- implementing flip function

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where
    g x y = f y x

-- map function
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- filter function

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x = x : filter' f xs | otherwise = filter f xs

-- largest number under divisible number by 3928
largestDiv :: Integer
largestDiv = head (filter' f [100000, 99999 ..])
  where
    f x = x `mod` 3820 == 0

-- creating a collatz chain
chainCollatz :: (Integral a) => a -> [a]
chainCollatz 1 = [1]
chainCollatz x | odd x = x : chainCollatz (x * 3 + 1) | even x = x : chainCollatz (x `div` 2)

-- lambdas
lengthMapping :: (Foldable t) => p -> [t a] -> [Int]
lengthMapping x = map length

main :: IO ()
main = do
  putStrLn "Hello chapter 5"