charName :: Char -> String
charName 'a' = "Albert"

-- pattern matching with list and list comprehensions

listCompre :: (Num a) => [(a, a)] -> [a]
listCompre xs = [a + b | (a, b) <- xs]

main = do
  putStrLn "hellp chapter 3"