import Control.Applicative (ZipList (getZipList))
import Data.Foldable qualified as F
import Data.Sequence (Seq (Empty))
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

newtype MyProduct a = MyProduct {getMyProduct :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

-- instance (Num a) => Monoid (MyProduct a) where
--   mempty = MyProduct 1
--   MyProduct x `mappend` MyProduct y = MyProduct (x * y)

data MyTree a = EmptyTree | Node a (MyTree a) (MyTree a) deriving (Show)

instance F.Foldable MyTree where
  foldMap :: (Monoid m) => (a -> m) -> MyTree a -> m
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree :: MyTree Integer
testTree = Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))

-- ghci> F.foldl (+) 0 testTree
-- 42

main :: IO ()
main = do
  putStrLn "Hello, world!"

-- In Haskell, a qualified import means that you're importing a module in such a way that you must prefix the functions from that module with the module name. This is useful when you have functions with the same name in different modules and you want to avoid name clashes.

-- Here's an example:

-- Map
-- In this example, Data.Map is imported in a qualified way. This means that if you want to use the lookup function from Data.Map, you have to write Map.lookup instead of just lookup.

-- You can also give the module a different name using as. In the example above, Data.Map is given the name Map. This is useful if the module name is long and you want to use a shorter name in your code.

-- In summary, a qualified import in Haskell allows you to avoid name clashes by requiring you to prefix the functions from a module with the module name.V¨