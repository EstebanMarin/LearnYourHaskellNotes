main :: IO ()
doubleMe :: (Num a) => a -> a
doubleMe x = x + 1

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

test = filter odd [10 .. 20]

other = map doubleMe (filter odd [10 .. 20])

doubleBigger :: (Ord a, Num a) => a -> a
doubleBigger x = if x > 100 then x else x * 2

listConcat = [1, 2, 3, 4] ++ [4, 5, 6, 7]

-- cons operator

testString :: [Char]
testString = 'A' : "small cat"

testInt :: [Int]
testInt = 5 : [8, 9]

comparingList =
  [3, 2, 3] < [3, 2, 5]

--   GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
-- ghci> [3, 2, 3] < [3, 2, 5]
-- True
-- ghci> [3, 2, 3] < [3, 2, 2]
-- False
-- ghci> [3, 2, 3] < [3, 2, 3]
-- False
-- ghci> [3, 2, 3] < [3, 1, 3]
-- False
-- ghci> [3, 2, 3] < [3, 3, 3]
-- True
-- ghci> [3, 2, 3] < [3, 3, 2]

-- ghci> [x*2 | x <- [1 ..15], x > 12]
-- [26,28,30]

boomBags xs = [if x > 10 then "boom" else "bang" | x <- xs, odd x, x < 20]

result = boomBags [7 .. 25]

-- ["bang","bang","boom","boom","boom","boom","boom"]

nouns = ["dog", "frog", "pope"]

adjetives = ["lazy", "grouchy", "scheming"]

combination = [a ++ " . " ++ n | n <- nouns, a <- adjetives]

-- ghci> combination
-- ["lazy . dog","grouchy . dog","scheming . dog","lazy . frog","grouchy . frog","scheming . frog","lazy . pope","grouchy . pope","scheming . pope"]

length' :: (Num a) => [t] -> [a]
length' xs = [1 | _ <- xs]

isInList = 4 `elem` [1 .. 10]

-- Using Tuples
touple1 = zip [1 ..] ['1', '2']

-- ghci> zip [1 ..] ['1', '2']
-- [(1,'1'),(2,'2')]

triangle = [(x, y, z) | x <- [1 .. 10], y <- [1 .. 10], z <- [1 .. 10], x ^ 2 + y ^ 2 == z ^ 2, x + y + z == 24]

main = do
  putStrLn "Hello, everybody!"
  putStrLn
    ( "Please look at my favorite odd numbers: "
        ++ show
          ( filter
              odd
              [10 .. 20]
          )
    )