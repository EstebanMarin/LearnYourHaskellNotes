-- ghci> factorial n = product [1 .. n]
-- ghci> :t factorial
-- factorial :: (Num a, Enum a) => a -> a
-- ghci> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000

-- inspecting types
-- ghci> factorial n = product [1 .. n]
-- ghci> :t factorial
-- factorial :: (Num a, Enum a) => a -> a
-- ghci> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000
-- ghci> :t head
-- head :: GHC.Stack.Types.HasCallStack => [a] -> a
-- ghci> :t fst
-- fst :: (a, b) -> a
-- ghci> fst (1,2)
-- 1
-- ghci> :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- every infix operator will have a typeclass

-- type annotations
-- ghci> read "True"

-- *** Exception: Prelude.read: no parse

-- ghci> read "True" || False
-- True
-- ghci> :t read
-- read :: Read a => String -> a
-- ghci> :t read
-- ghci> read "5" :: Int
-- 5
-- ghci> read "5" :: Float

main :: IO ()
main = do
  putStrLn "Hello, everybody!"