main :: IO ()
main = do
  putStrLn "todo list"
  todoItem <- getLine
  appendFile "todofil.txt" (todoItem ++ "\n")