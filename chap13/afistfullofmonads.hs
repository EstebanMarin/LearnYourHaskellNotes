-- functor
class FunctorMine f where
  fmap1 :: (a -> b) -> f a -> f b

class (FunctorMine f) => ApplicativeMine f where
  pure1 :: a -> f a
  (<*!>) :: f (a -> b) -> f a -> f b

class (ApplicativeMine m) => MonadMine m where
  return :: a -> m a

  -- it takes a monadic value and a function that takes a normal value and returns a monadic value
  -- and returns a monadic value
  (>>=!) :: m a -> (a -> m b) -> m b
  (>>!) :: m a -> m b -> m b
  x >>! y = x >>=! const y

main :: IO ()
main = do
  putStrLn "A fistful of monads"