import Control.Monad (forever)
import Data.Char (toUpper)
import GHC.Data.ShortText (ShortText (contents))
import System.IO

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

main :: IO ()
main =
  forever $ do
    l <- getLine
    putStrLn $ map toUpper l
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents