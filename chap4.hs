import Distribution.Simple.Setup (falseArg)

test = last [1, 2, 3, 4]

-- recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "error of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

-- more recursive functions

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [i] -> [i]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: t -> [t]
repeat' x = x : repeat' x

-- wow so nice!|
-- ghci> take' 9 (repeat' 3)
-- [3,3,3,3,3,3,3,3,3]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

element' :: (Eq i) => i -> [i] -> Bool
element' _ [] = False
element' a (x : xs)
  | a == x = True
  | otherwise = element' a xs

main :: IO ()
main = do
  putStrLn "Hello chapter 4"