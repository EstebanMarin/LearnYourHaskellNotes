import Control.Applicative (ZipList (getZipList))
import Language.Haskell.TH (Lit (CharL))

-- new types

applicativeTest :: [Integer]
applicativeTest = [(* 2), (+ 3)] <*> [1, 2, 3]

-- say we want to map differently

-- applicativeTest2 = getZipList $ ZipList [(* 2), (+ 3), (+ 4)] <*> ZipList [1, 2, 3]

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

-- ghci> getPair $ fmap (*100) (Pair(2,3))
-- (200,3)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool = CoolBool {getCoolBool :: Bool}

newtype CoolBool2 = CoolBoo2 {getCoolBool2 :: Bool}

-- In summary, use type for creating type synonyms (no new type is created),
-- newtype for creating new types with a single constructor (guaranteed no additional runtime overhead),
-- and data for creating new types with one or more constructors (can have additional runtime overhead).

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

helloMe2 :: CoolBool2 -> String
helloMe2 (CoolBoo2 _) = "hello"

main :: IO ()
main = do
  putStrLn "Hello, world!"