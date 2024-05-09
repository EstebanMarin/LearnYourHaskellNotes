import Control.Arrow (ArrowApply (app))
import Data.Binary.Put (PutM (Put))
import Data.Type.Equality (apply)

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

main :: IO ()
main = do
  putStrLn "few monads more"