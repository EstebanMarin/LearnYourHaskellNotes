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