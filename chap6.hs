import Data.List

nob2 :: (Eq a) => [a] -> Int
nob2 = length . nub

testnob2 = nob2 [1, 1, 1, 1, 1]

-- counting words from a string

countWord :: String -> [(String, Int)]
countWord = map (\ws -> (head ws, length ws)) . group . sort . words

testingCountWord :: [(String, Int)]
testingCountWord = countWord "hey these are the words in the sentence"

main :: IO ()
main = do
  putStrLn "Hello chapter 6"