-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- applicative functor

class FunctorMine f where
  fmap1 :: (a -> b) -> f a -> f b

class (FunctorMine f) => ApplicativeMine f where
  pure1 :: a -> f a
  (<*!>) :: f (a -> b) -> f a -> f b

-- Example 1: Using <*> with Maybe
example1 :: MyMaybe String
example1 = fmap1 (++ " World") (MyJust "Hello") -- Result: Just "Hello World"

-- Example 2: Using <*> with List
example2 :: [Int]
example2 = fmap1 (*) [1, 2, 3] <*> [10, 100] -- Result: [10,100,20,200,30,300]

-- Example 3: Using <*> with IO
example3 :: IO ()
example3 = fmap1 (++ "Hello, World!") (pure1 "Hello, World!") >>= putStrLn -- This will print "Hello, World!" to the console

-- Example 4: Using <*> with custom Applicative
data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance FunctorMine MyMaybe where
  fmap1 :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap1 _ MyNothing = MyNothing
  fmap1 f (MyJust x) = MyJust (f x)

instance ApplicativeMine MyMaybe where
  pure1 :: a -> MyMaybe a
  pure1 = MyJust
  (<*!>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  MyNothing <*!> _ = MyNothing
  (MyJust f) <*!> something = fmap1 f something

instance FunctorMine [] where
  fmap1 :: (a -> b) -> [a] -> [b]
  fmap1 f xs = [f x | x <- xs]

instance ApplicativeMine [] where
  pure1 :: a -> [a]
  pure1 x = [x]
  (<*!>) :: [a -> b] -> [a] -> [b]
  fs <*!> xs = [f x | f <- fs, x <- xs]

instance FunctorMine IO where
  fmap1 :: (a -> b) -> IO a -> IO b
  fmap1 f action = do
    f <$> action

instance ApplicativeMine IO where
  pure1 :: a -> IO a
  pure1 = return
  (<*!>) :: IO (a -> b) -> IO a -> IO b
  a <*!> b = do
    f <- a
    f <$> b

example4 :: MyMaybe Int
example4 = MyJust (* 2) <*!> MyJust 10 -- Result: MyJust 20

main :: IO ()
main = do
  putStrLn "hello chapter 11"