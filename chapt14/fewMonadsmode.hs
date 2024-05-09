import Control.Arrow (ArrowApply (app))
import Data.Binary.Put (PutM (Put))
import Data.Monoid
import Data.Type.Equality (apply)
import Distribution.Simple.Test.Log (TestSuiteLog (testSuiteName))

-- writer monad

-- lets start with a simple example logging the steps of a computation

isBigGang :: Int -> Bool
isBigGang x = x > 9

-- ghci> (3, "Smallish gang") `applyLog` isBigGang'
-- (False,"Smallish gangCompared gang size to 9")
isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compared gang size to 9")

isBigGang'' :: Int -> (Bool, String)
isBigGang'' x = (x > 9, "Compared gang size to 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

test :: (Bool, String)
test = (3, "Smallish gang") `applyLog` isBigGang'

test2 :: (Int, String)
test2 = ("Tobin", "Got outlaw name") `applyLog` (\x -> (length x, "Applied length"))

test3 :: (Int, String)
test3 = ("Bathcat", "Got outlaw name") `applyLog` (\x -> (length x, "Applied length"))

-- ghci> test3
-- (7,"Got outlaw nameApplied length")
-- ghci> test2
-- (5,"Got outlaw nameApplied length")
-- ghci> test
-- (False,"Smallish gangCompared gang size to 9")

-- Monoids to the rescue

applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

testAddDrink :: (Food, Price)
testAddDrink = ("beans", Sum 10) `applyLog'` addDrink

-- ghci> testAddDrink
-- ("milk",Sum {getSum = 35})

newtype Writer w a = Writer {runWriter :: (a, w)}

testSum :: Sum Integer
testSum = Sum 3 `mappend` Sum 9

-- functor
class FunctorMine f where
  fmap1 :: (a -> b) -> f a -> f b

class (FunctorMine f) => ApplicativeMine f where
  pure1 :: a -> f a
  (<*!>) :: f (a -> b) -> f a -> f b

class (ApplicativeMine m) => MonadMine m where
  return1 :: a -> m a
  fail1 :: String -> m a
  (>>=!) :: m a -> (a -> m b) -> m b
  (>>!) :: m a -> m b -> m b
  x >>! y = x >>=! const y

class MonoidMine m where
  mempty1 :: m
  mappend1 :: m -> m -> m

  -- Optionally, you can provide a default implementation for mconcat1
  mconcat1 :: [m] -> m
  mconcat1 = foldr mappend1 mempty1

newtype WriterMine w a = WriterMine {runWriter1 :: (a, w)}

-- setting up the implementation

-- instance (MonoidMine w) => MonadMine (WriterMine w)

-- return1 x = runWriter1 (x, mempty1)

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

main :: IO ()
main = do
  putStrLn "few monads more"