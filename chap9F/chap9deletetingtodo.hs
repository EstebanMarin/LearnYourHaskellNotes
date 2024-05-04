import Control.Exception
import Data.List
import System.Directory
import System.IO

main :: IO ()
main =
  do
    contents <- readFile "todofil.txt"
    let todoTasks = lines contents
        numberedTask = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
    putStrLn "These are the TODO-items"
    mapM_ putStrLn numberedTask
    putStrLn "Which one do you want to remove"
    numberString <- getLine
    let number = read numberString
        newTodoItem = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError
      (openTempFile "." "temp")
      ( \(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName
      )
      ( \(tempName, tempHandle) -> do
          hPutStr tempHandle newTodoItem
          hClose tempHandle
          removeFile "todofil.txt"
          renameFile tempName "todofil.txt"
      )
    -- (tempName, tempHandle) <- openTempFile "." "temp"
    putStrLn "test"

-- bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- bracketOnError acquire release use = mask $ \restore -> do
--   a <- acquire
--   r <- restore (use a) `onException` release a
--   _ <- release a
-- return r
-- bracketOnError

-- (openTempFile "." "temp")
-- ( \(tempName, tempHandle) -> do
--     hClose tempHandle
--     removeFile tempName
--   )
--   ( \(tempName, tempHandle) -> do
--       hPutStr tempHandle newTodoItem
--       hClose tempHandle
--       removeFile "todofil.txt"
--       renameFile tempName "todofil.txt"
--   )

-- (tempName, tempHandle) <- openTempFile "." "temp"
-- hPutStr tempHandle newTodoItem
-- hClose tempHandle
-- removeFile "todofil.txt"
-- renameFile tempName "todofil.txt"
