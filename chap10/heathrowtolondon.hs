-- representing the road system as a list of sections
data Section = Section {a :: Int, b :: Int, c :: Int} deriving (Show)

type RoadSystem = [Section]

heathrowtoLondon :: RoadSystem
heathrowtoLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

-- the optimal path is the one that has the least cost
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
   in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA =
        if forwardPriceToA <= crossPriceToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if forwardPriceToB <= crossPriceToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (newPathToA, newPathToB)

main :: IO ()
main = do
  putStrLn "Heathrow to London"