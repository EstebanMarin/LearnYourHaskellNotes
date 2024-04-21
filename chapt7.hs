data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

-- deriving (Show)

area :: Shape -> Float
area (Circle _ r) = r * pi ^ 2
area (Rectangle (Point x1 x2) (Point y1 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) a b =
  Circle (Point (x1 + a) (y1 + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

--

-- Typeclass Eq from standard library
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--  ....

data TraficLight = Red | Yellow | Green

instance Eq TraficLight where
  (==) :: TraficLight -> TraficLight -> Bool
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False
  (/=) :: TraficLight -> TraficLight -> Bool
  Red /= Red = False
  Green /= Green = False
  Yellow /= Yellow = False
  _ /= _ = True

instance Show TraficLight where
  show :: TraficLight -> String
  show Red = "This is Red"
  show Green = "This is Green"
  show Yellow = "This is Yellow"

main :: IO ()
main = do
  putStrLn "Hello chapter 7"