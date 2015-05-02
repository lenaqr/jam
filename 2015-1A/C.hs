import Control.Monad
import Text.Printf
import Data.List
import Data.Function (on)
import qualified Data.Set as Set

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [(Int, Int)])
type Answer = [Int]

solve :: Case -> Answer
solve (_, trees) = allBoundaryDistances trees (allDirections trees)

-- count the min number of trees from the boundary in the given direction
boundaryDistance :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Int
boundaryDistance ps v p =
  let nv = normal v
      d = dot nv p
  in sum [1 | p' <- ps, dot nv p' > d]

-- smallest distance
minBoundaryDistance :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
minBoundaryDistance ps vs p =
  case vs of
    [] -> 0
    _ -> minimum [boundaryDistance ps v p | v <- vs]

-- smallest distance for each point
allBoundaryDistances :: [(Int, Int)] -> [(Int, Int)] -> [Int]
allBoundaryDistances ps vs = [minBoundaryDistance ps vs p | p <- ps]

-- get all possible directions
allDirections :: [(Int, Int)] -> [(Int, Int)]
allDirections ps =
  [diff p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]

-- actually just returns the vectors
convexHull :: [(Int, Int)] -> [(Int, Int)]
convexHull ps =
  let p0 = minimum ps
      f p v =
        let (_, _, p'', v'') = maximum [(sim v (diff p p'), (- mag (diff p p')), p', diff p p') | p' <- ps, p' /= p]
        in p'' : (if p'' == p0 then [] else f p'' v'')
  in f p0 (0,1)

-- utility

diff :: (Int, Int) -> (Int, Int) -> (Int, Int)
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

dot :: (Int, Int) -> (Int, Int) -> Integer
dot (x1, y1) (x2, y2) = fromIntegral (x1 * x2 + y1 * y2)

mag :: (Int, Int) -> Integer
mag v = dot v v

sim :: (Int, Int) -> (Int, Int) -> Rational
sim v1 v2 = let d12 = dot v1 v2
            in fromIntegral (d12 * d12 * signum d12) / fromIntegral (mag v1 * mag v2)

normal :: (Int, Int) -> (Int, Int)
normal (x, y) = (-y, x)

readCase :: IO Case
readCase = do
  n <- readLn
  trees <- replicateM n $ do
    [x, y] <- readWords
    return (x, y)
  return (n, trees)

printCase :: Int -> Answer -> IO ()
printCase x ys = do
  printf "Case #%d:\n" x
  mapM_ (printf "%d\n") ys

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
