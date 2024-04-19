import Data.Char
import Data.List (find, group, isPrefixOf, nub, sort, tails)
import Data.Map qualified as Map

-- import Data.Map

nob2 :: (Eq a) => [a] -> Int
nob2 = length . nub

testnob2 = nob2 [1, 1, 1, 1, 1]

-- counting words from a string

countWord :: String -> [(String, Int)]
-- I see here partial applying a function of map, just provind a composed function
-- now understanding the order is whats causing me pain
-- well I should read it from right to left
-- ghci> map (\ws -> (head ws, length ws)) [["different"],["hey"],["words","words"]]
-- map is the base layer and we provide functions to compose
-- I see just 1 parameter functions
countWord = map (\ws -> (head ws, length ws)) . group . sort . words

testingCountWord :: [(String, Int)]
testingCountWord = countWord "hey these are the words in the sentence"

-- Needle in a haystack

test = "hawai" `isPrefixOf` "hawaii joe"

test2 = any (> 4) [1, 2, 3, 4]

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- ghci> "ta" `isIn` "tada"
-- True
testOrd = ord 'a'

testChar = chr 97

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode offset = map (\c -> chr $ ord c - offset)

-- ghci> encode 7 "esteban"
-- "lz{lihu"
-- ghci> :reload
-- [1 of 2] Compiling Main             ( chap6.hs, interpreted ) [Source file changed]
-- Ok, one module loaded.
-- ghci> decode 7 "lz{lihu"

-- first one to x

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstOntoA :: Int -> Maybe Int
firstOntoA a = find (\x -> digitSum x == a) [1 ..]

-- ghci> firstOntoA 40
-- Just 49999

testSnd :: (a, b) -> b
testSnd = snd

phoneBook :: [(String, String)]
phoneBook =
  [ ("betty", "555-4567"),
    ("bonnie", "452-3645"),
    ("lucille", "493-5243"),
    ("wendy", "853-4123")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\x -> key == fst x)

-- findKey key = snd . head . filter (\(k, v) -> key == k)

main :: IO ()
main = do
  putStrLn "Hello chapter 6"