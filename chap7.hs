import Data.Map qualified as Map

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

data Vector a = Vector a a a deriving (Show)

-- Locker State

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lNumber map = case Map.lookup lNumber map of
  Nothing -> Left $ "Locker " ++ show lNumber ++ " doesn't exists!"
  Just (state, code) -> if state /= Taken then Right code else Left $ "Locker: " ++ show lNumber ++ " is already taken"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZGS123")),
      (101, (Taken, "HWRF14")),
      (102, (Free, "GSDFB5")),
      (103, (Taken, "VADW43"))
    ]

-- recursive data structures
-- data TreeMine a
--   = EmptyTree
--   | Node a (TreeMine a) (TreeMine a)
--   deriving (Show)

-- singleton :: a -> Tree a
-- singleton x = Node x EmptyTree EmptyTree

data TreeMine a = EmptyTree | NodeMine a (TreeMine a) (TreeMine a) deriving (Show)

singleton :: a -> TreeMine a
singleton a = NodeMine a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> TreeMine a -> TreeMine a
treeInsert x EmptyTree = NodeMine x EmptyTree EmptyTree
treeInsert x (NodeMine a left right)
  | x == a = NodeMine a left right
  | x < a = NodeMine a (treeInsert x left) right
  | x > a = NodeMine a left (treeInsert x right)

treeElem :: (Ord a) => a -> TreeMine a -> Bool
treeElem x EmptyTree = False
treeElem x (NodeMine a left right)
  | x == a = True
  | x < a = treeElem a left
  | x > a = treeElem a right

nums :: [Integer]
nums = [8, 6, 4, 1, 7, 3, 1]

numsTrees :: TreeMine Integer
numsTrees = foldr treeInsert EmptyTree nums

instance Functor TreeMine where
  fmap :: (a -> b) -> TreeMine a -> TreeMine b
  fmap f EmptyTree = EmptyTree
  fmap f (NodeMine x left right) = NodeMine (f x) (fmap f left) (fmap f right)

mineTreeInt :: TreeMine Integer
mineTreeInt = fmap (+ 1) numsTrees

-- Either as a Functor
-- Either Data type

data EitherMine a b = LeftMine a | RightMine b deriving (Show, Read, Ord, Eq)

main :: IO ()
main = do
  putStrLn "Hello chapter 7"