charName :: Char -> String
charName 'a' = "Albert"

-- pattern matching with list and list comprehensions

listCompre :: (Num a) => [(a, a)] -> [a]
listCompre xs = [a + b | (a, b) <- xs]

-- Gards

bmiTell :: Double -> Double -> String
bmiTell w h
  | bmi < 2 = "You are fat"
  | bmi > 3 = "You are fatter"
  where
    bmi = w / h ^ 2

calcBmis :: (Fractional a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi w h = w / h ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- let in list comprehensions

calcBmis2 :: (Fractional a, Ord a) => [(a, a)] -> [a]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 0.25]

-- case

-- how can I make it exaustive?
describeList :: [a] -> [Char]
describeList xs = "The list is " ++ case xs of [] -> "empty"

main :: IO ()
main = do
  putStrLn "hello chapter 3"