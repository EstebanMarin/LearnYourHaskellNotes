import Distribution.System (OS)

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

-- it takes a monadic value and a function that takes a normal value and returns a monadic value
-- and returns a monadic value
-- (>>=!) :: m a -> (a -> m b) -> m b
-- (>>!) :: m a -> m b -> m b

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

bindMyMaybe :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
bindMyMaybe MyNothing _ = MyNothing
bindMyMaybe (MyJust x) f = f x

instance MonadMine MyMaybe where
  return1 :: a -> MyMaybe a
  return1 = MyJust
  fail1 :: String -> MyMaybe a
  fail1 _ = MyNothing
  (>>=!) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (>>=!) = bindMyMaybe

-- ghci> return1 "What" :: MyMaybe String
-- MyJust "What"

-- ghci> MyJust 9 >>=! \x -> return1(x*9)
-- MyJust 81

type Bird = Int

type Pole = (Bird, Bird)

landLeft :: Bird -> Pole -> MyMaybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = MyJust (left + n, right)
  | otherwise = MyNothing

landRight :: Bird -> Pole -> MyMaybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = MyJust (left, right + n)
  | otherwise = MyNothing

-- ghci> MyNothing >>=! landLeft 2
-- MyNothing

banana :: Pole -> MyMaybe Pole
banana _ = MyNothing

test :: MyMaybe Pole
test = return1 (0, 0) >>=! landLeft 1 >>=! landRight 4 >>=! landLeft (-1) >>=! landRight (-2) >>=! banana >>=! landLeft 1 >>=! landRight 2

-- do notation
-- instance Monad oy MyMaybe
instance Functor MyMaybe where
  fmap = fmap1

instance Applicative MyMaybe where
  pure = return1
  (<*>) = (<*!>)

instance Monad MyMaybe where
  return :: a -> MyMaybe a
  return = return1
  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (>>=) = (>>=!)

foo :: MyMaybe String
foo = do
  x <- MyJust 3
  y <- MyJust "!"
  return1 (show x ++ y)

routine :: MyMaybe Pole
routine = do
  start <- return1 (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

main :: IO ()
main = do
  putStrLn "A fistful of monads"