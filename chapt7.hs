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

main :: IO ()
main = do
  putStrLn "Hello chapter 7"