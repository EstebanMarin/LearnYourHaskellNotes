import Data.Char

main :: IO ()
main = do
  putStrLn "Hello chapter 8"
  putStrLn "Whats your first name?"
  firstName <- getLine
  putStrLn "Whats your last name?"
  lastName <- getLine
  let upperAll = map toUpper
      firstUpper = upperAll firstName
      lastUpper = upperAll lastName
  putStrLn $ "hello: " ++ firstUpper ++ " __ " ++ lastUpper