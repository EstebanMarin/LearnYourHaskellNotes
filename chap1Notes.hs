main :: IO ()
doubleMe :: (Num a) => a -> a
doubleMe x = x + 1

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

test = filter odd [10 .. 20]

other = map doubleMe (filter odd [10 .. 20])

doubleBigger :: (Ord a, Num a) => a -> a
doubleBigger x = if x > 100 then x else x * 2

main = do
  putStrLn "Hello, everybody!"
  putStrLn
    ( "Please look at my favorite odd numbers: "
        ++ show
          ( filter
              odd
              [10 .. 20]
          )
    )